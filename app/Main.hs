module Main where

import Data.Bool (Bool (False, True), bool)

import Data.List (sort)
import Data.Maybe (fromMaybe, maybe)

main :: IO ()
main = do
    let foo = fromMaybe bool Nothing
    print $ (foo undefined undefined undefined :: Int)
    print True
-- main = parseFile
