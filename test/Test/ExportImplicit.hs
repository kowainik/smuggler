module Test.ExportImplicit where

isExported = (+1)

alsoExported = True

main :: IO ()
main = print $ isExported 1
