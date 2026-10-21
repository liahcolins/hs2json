module Main (main) where

import Control.Monad (unless)
import System.Exit (exitFailure)
import Test.QuickCheck

import SimpleJSON
import qualified PrettyJSON as Pretty
import Prettify (pretty)

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

prop_renderStringEscapes :: Bool
prop_renderStringEscapes =
    renderForTest (JString "linha\ncoluna\t\"\\")
        == "\"linha\\ncoluna\\t\\\"\\\\\""

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
        , runProperty "o renderizador escapa caracteres especiais"
            prop_renderStringEscapes
        ]

    unless (all isSuccess results) exitFailure