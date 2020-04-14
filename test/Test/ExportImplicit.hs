module Test.ExportImplicit where

import Data.Char

isExported = (+1)

alsoExported = map toLower "AbC"

main :: IO ()
main = print $ isExported 1
