
module Prettify
    ( Doc
    , empty
    , char
    , text
    , double
    , line
    , (<>)
    , hcat
    , fsep
    , (</>)
    , punctuate
    , group
    , softline
    , compact
    , pretty
    , fill
    , nest
    ) where

import Prelude hiding ((<>))

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         | Nest Int Doc
         deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

nest :: Int -> Doc -> Doc
nest amount doc = Nest amount doc

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep [] = empty
fsep [d] = d
fsep (d:ds) = d <> softline <> fsep ds

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten (Nest n d) = Nest n (flatten d)
flatten other = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

compact :: Doc -> String
compact x = transform [x]
  where
    transform [] = ""
    transform (d:ds) =
        case d of
            Empty        -> transform ds
            Char c       -> c : transform ds
            Text s       -> s ++ transform ds
            Line         -> '\n' : transform ds
            a `Concat` b -> transform (a:b:ds)
            Nest _ doc   -> transform (doc:ds)
            _ `Union` b  -> transform (b:ds)

fill :: Int -> Doc -> Doc
fill width doc =
    doc <> text (replicate (max 0 (width - lineWidth (compact doc))) ' ')
  where
    lineWidth s = length (takeWhile (/= '\n') (reverse s))

pretty :: Int -> Doc -> String
pretty width x = best 0 [(x, 0)]
  where
    best col ((d, indent):ds) =
        case d of
            Empty -> best col ds
            Char c -> c : best (col + 1) ds
            Text s -> s ++ best (col + length s) ds
            Line ->
                '\n' : (replicate (max 0 indent) ' ' ++ best (max 0 indent) ds)
            a `Concat` b ->
                best col ((a, indent):(b, indent):ds)
            a `Union` b ->
                nicest col
                    (best col ((a, indent):ds))
                    (best col ((b, indent):ds))
            Nest n doc ->
                best col ((doc, max 0 (indent + n)):ds)

    best _ [] = ""

    nicest col a b
        | (width - least) `fits` a = a
        | otherwise = b
      where
        least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
_ `fits` "" = True
_ `fits` ('\n':_) = True
w `fits` (_:cs) = (w - 1) `fits` cs
