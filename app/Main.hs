module Main where

import Data.List (sort)
import Data.Maybe (fromMaybe, maybe)

foo = False

main :: IO ()
main = print $ fromMaybe True Nothing
-- main = parseFile
