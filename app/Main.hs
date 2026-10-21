module Main (main) where

import SimpleJSON (JValue(..))
import PrettyJSON (renderJValue)
import Prettify (pretty)

main :: IO ()
main = putStrLn (pretty 80 (renderJValue example))
  where
    example :: JValue
    example = JObject [("foo", JNumber 1), ("bar", JBool False)]