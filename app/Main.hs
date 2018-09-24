module Main where

import Data.Bool (Bool ( True), bool)
import Data.Maybe (fromMaybe,)

main :: IO ()
main = do
    let foo = fromMaybe bool Nothing
    print $ (foo undefined undefined undefined :: Int)
    print True
-- main = parseFile
