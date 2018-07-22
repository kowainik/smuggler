module Main where

import Data.Foldable (for_)
import System.Directory (listDirectory, removeDirectoryRecursive, removeFile)
import System.Exit (exitFailure)
import System.FilePath (isExtensionOf, splitExtension, (<.>), (</>))

main :: IO ()
main = do
    files <- map ("test/Test" </>) <$> listDirectory "test/Test/"
    let testFiles = filter ("test" `isExtensionOf`) files
    -- compare outputs of .test and .golden files
    result <- traverse compareWithGolden testFiles >>= pure . foldr (&&) True


    -- clean up all .test files
    putStrLn "Cleaning tests"
    for_ testFiles removeFile

    -- clean up .smuggler/ cache directory
    putStrLn "Cleaning smuggler cache"
    removeDirectoryRecursive ".smuggler"

    if result
        then putStrLn "Success"
        else putStrLn "Fail" >> exitFailure
  where
    compareWithGolden :: FilePath -> IO Bool
    compareWithGolden f = do
        let (name, _) = splitExtension f
        goldenContent <- readFile $ name <.> "golden"
        testContent   <- readFile f
        pure $ goldenContent == testContent
