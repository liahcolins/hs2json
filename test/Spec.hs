module Main (main) where

import Control.Monad (unless)
import System.Exit (exitFailure)
import Test.QuickCheck

import SimpleJSON
import qualified PrettyJSON as Pretty
import Data.List (intercalate)
import Prettify
    ( Doc
    , char
    , compact
    , double
    , empty
    , fill
    , group
    , hcat
    , line
    , nest
    , pretty
    , punctuate
    , text
    )

newtype TestDoc = TestDoc Doc
    deriving (Show)

instance Arbitrary TestDoc where
    arbitrary = TestDoc <$> sized genDoc

genDoc :: Int -> Gen Doc
genDoc n
    | n <= 0 = oneof
        [ pure empty
        , char <$> arbitrary
        , text <$> arbitrary
        , pure line
        , double <$> arbitrary
        ]
    | otherwise = frequency
        [ (2, pure empty)
        , (2, char <$> arbitrary)
        , (3, text <$> arbitrary)
        , (1, pure line)
        , (1, double <$> arbitrary)
        , (2, do
              x <- genDoc (n `div` 2)
              y <- genDoc (n `div` 2)
              pure (hcat [x, y])
          )
        , (1, group <$> genDoc (n `div` 2))
        ]

-- a instancia fica no modulo de testes para manter QuickCheck fora das dependencias da biblioteca principal
instance Arbitrary JValue where
    arbitrary = sized genJValue

genJValue :: Int -> Gen JValue
genJValue size
    | size <= 0 = oneof
        [ JString <$> arbitrary
        , JNumber <$> choose (-1000000.0, 1000000.0)
        , JBool <$> arbitrary
        , pure JNull
        ]
    | otherwise = frequency
        [ (4, JString <$> arbitrary)
        , (3, JNumber <$> choose (-1000000.0, 1000000.0))
        , (2, JBool <$> arbitrary)
        , (2, pure JNull)
        , (1, JArray <$> resize (size `div` 2)
              (listOf (genJValue (size `div` 2)))
        )
        , (1, JObject <$> resize (size `div` 2)
              (listOf (genField (size `div` 2)))
        )
        ]

genField :: Int -> Gen (String, JValue)
genField size = do
    key <- arbitrary
    value <- genJValue size
    pure (key, value)

prop_getStringRoundTrip :: String -> Bool
prop_getStringRoundTrip s =
    getString (JString s) == Just s

prop_getDoubleRoundTrip :: Int -> Bool
prop_getDoubleRoundTrip n =
    let number = fromIntegral n + 0.125 :: Double
    in getDouble (JNumber number) == Just number

prop_getIntTruncates :: Property
prop_getIntTruncates =
    forAll (choose (-1000000, 1000000) :: Gen Int) $ \n ->
        let fraction = if n >= 0 then 0.75 else -0.75
            number = fromIntegral n + fraction :: Double
        in getInt (JNumber number) == Just n

prop_getBoolRoundTrip :: Bool -> Bool
prop_getBoolRoundTrip b =
    getBool (JBool b) == Just b

prop_getArrayRoundTrip :: [JValue] -> Bool
prop_getArrayRoundTrip values =
    getArray (JArray values) == Just values

prop_getObjectRoundTrip :: [(String, JValue)] -> Bool
prop_getObjectRoundTrip fields =
    getObject (JObject fields) == Just fields

prop_isNullConsistent :: JValue -> Bool
prop_isNullConsistent value =
    case value of
        JNull -> isNull value
        _     -> not (isNull value)

renderForTest :: JValue -> String
renderForTest = pretty 100000 . Pretty.renderJValue

prop_renderNull :: Bool
prop_renderNull =
    renderForTest JNull == "null"

prop_renderBool :: Bool -> Bool
prop_renderBool b =
    renderForTest (JBool b) == (if b then "true" else "false")

prop_renderNumber :: Int -> Bool
prop_renderNumber n =
    let number = fromIntegral n :: Double
    in renderForTest (JNumber number) == show number

prop_renderEmptyArray :: Bool
prop_renderEmptyArray =
    renderForTest (JArray []) == "[]"

prop_renderEmptyObject :: Bool
prop_renderEmptyObject =
    renderForTest (JObject []) == "{}"

prop_renderArrayWithValues :: Bool
prop_renderArrayWithValues =
    renderForTest (JArray [JNumber 1, JBool False])
        == "[1.0, false]"

prop_renderObjectWithFields :: Bool
prop_renderObjectWithFields =
    renderForTest (JObject [("foo", JNumber 1), ("bar", JBool False)])
        == "{\"foo\": 1.0, \"bar\": false}"

prop_renderUnicodeEscapes :: Bool
prop_renderUnicodeEscapes =
    renderForTest (JString ['\x732b', '\x1f600'])
        == "\"\\u732b\\ud83d\\ude00\""

prop_renderObjectIndentation :: Bool
prop_renderObjectIndentation =
    pretty 15
        (Pretty.renderJValue
            (JObject [("foo", JNumber 1), ("bar", JBool False)]))
        == "{\"foo\": 1.0,\n  \"bar\": false}"

prop_renderStringEscapes :: Bool
prop_renderStringEscapes =
    renderForTest (JString "linha\ncoluna\t\"\\")
        == "\"linha\\ncoluna\\t\\\"\\\\\""

prop_emptyId :: TestDoc -> Bool
prop_emptyId (TestDoc doc) =
    hcat [empty, doc] == doc
        && hcat [doc, empty] == doc

prop_char :: Char -> Bool
prop_char c =
    compact (char c) == [c]

prop_text :: String -> Bool
prop_text s =
    compact (text s) == s

prop_line :: Bool
prop_line =
    compact line == "\n"

prop_double :: Double -> Bool
prop_double d =
    compact (double d) == show d

prop_hcat :: [TestDoc] -> Bool
prop_hcat docs =
    compact (hcat (map unwrap docs))
        == concatMap (compact . unwrap) docs
  where
    unwrap (TestDoc doc) = doc

prop_punctuate :: TestDoc -> [TestDoc] -> Bool
prop_punctuate (TestDoc separator) docs =
    compact (hcat (punctuate separator (map unwrap docs)))
        == intercalate (compact separator) (map (compact . unwrap) docs)
  where
    unwrap (TestDoc doc) = doc

prop_fillPadsShortDocument :: Bool
prop_fillPadsShortDocument =
    compact (fill 5 (text "hi")) == "hi   "

prop_fillLeavesLongDocumentUnchanged :: Bool
prop_fillLeavesLongDocumentUnchanged =
    compact (fill 2 (text "hello")) == "hello"

prop_nestIndentsFollowingLine :: Bool
prop_nestIndentsFollowingLine =
    pretty 20 (nest 2 (hcat [text "a", line, text "b"]))
        == "a\n  b"

runProperty :: Testable prop => String -> prop -> IO Result
runProperty name propToRun = do
    putStrLn ("\nPropriedade: " ++ name)
    quickCheckResult propToRun

main :: IO ()
main = do
    results <- sequence
        [ runProperty "getString recupera a string original"
            prop_getStringRoundTrip
        , runProperty "getDouble recupera o numero original"
            prop_getDoubleRoundTrip
        , runProperty "getInt trunca a parte decimal"
            prop_getIntTruncates
        , runProperty "getBool recupera o booleano original"
            prop_getBoolRoundTrip
        , runProperty "getArray recupera os elementos originais"
            prop_getArrayRoundTrip
        , runProperty "getObject recupera os campos originais"
            prop_getObjectRoundTrip
        , runProperty "isNull identifica corretamente o nulo"
            prop_isNullConsistent
        , runProperty "o renderizador produz null"
            prop_renderNull
        , runProperty "o renderizador produz booleanos JSON"
            prop_renderBool
        , runProperty "o renderizador produz numeros"
            prop_renderNumber
        , runProperty "o renderizador produz arrays vazios"
            prop_renderEmptyArray
        , runProperty "o renderizador produz objetos vazios"
            prop_renderEmptyObject
        , runProperty "arrays com valores nao adicionam espacos finais"
            prop_renderArrayWithValues
        , runProperty "objetos com campos nao adicionam espacos finais"
            prop_renderObjectWithFields
        , runProperty "o renderizador escapa caracteres Unicode"
            prop_renderUnicodeEscapes
        , runProperty "objetos recebem indentacao em quebras de linha"
            prop_renderObjectIndentation
        , runProperty "o renderizador escapa caracteres especiais"
            prop_renderStringEscapes
        , runProperty "empty e identidade da concatenacao"
            prop_emptyId
        , runProperty "char preserva o caractere"
            prop_char
        , runProperty "text preserva a string"
            prop_text
        , runProperty "line representa uma quebra de linha"
            prop_line
        , runProperty "double usa a representacao show"
            prop_double
        , runProperty "hcat concatena documentos"
            prop_hcat
        , runProperty "punctuate intercala a pontuacao"
            prop_punctuate
        , runProperty "fill completa documentos curtos"
            prop_fillPadsShortDocument
        , runProperty "fill nao altera documentos mais largos"
            prop_fillLeavesLongDocumentUnchanged
        , runProperty "nest indenta linhas seguintes"
            prop_nestIndentsFollowingLine
        ]

    unless (all isSuccess results) exitFailure