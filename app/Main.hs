module Main where

import Data.Bool ( bool)
import Data.List ()
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    let foo = fromMaybe bool Nothing
    print $ (foo undefined undefined undefined :: Int)
    print True
-- main = parseFile
