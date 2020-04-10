module Main where

import Control.Monad (when)
import Data.Foldable (for_)
import Data.List (intercalate, isPrefixOf)
import System.Console.ANSI (Color (..), ColorIntensity (Dull), ConsoleIntensity (BoldIntensity),
                            ConsoleLayer (Foreground), SGR (..), setSGR)
import System.Directory (doesDirectoryExist, listDirectory, removeDirectoryRecursive, removeFile)
import System.Exit (exitFailure)
import System.FilePath (isExtensionOf, splitExtension, (<.>), (</>))

main :: IO ()
main = do
    files <- map ("test/Test" </>) <$> listDirectory "test/Test/"
    let testFiles = filter ("test" `isExtensionOf`) files
    -- compare outputs of .test and .golden files
    result <- and <$> traverse compareWithGolden testFiles

    -- clean up all .test files
    putStrLn "Cleaning tests"
    for_ testFiles removeFile

    -- TODO: redundant?
    -- clean up .smuggler/ cache directory
    cacheDirExists <- doesDirectoryExist ".smuggler"
    when cacheDirExists $ do
      putStrLn "Cleaning smuggler cache"
      removeDirectoryRecursive ".smuggler"

    if result
        then successMessage "Success"
        else failureMessage "Some tests failed" >> exitFailure
  where
    compareWithGolden :: FilePath -> IO Bool
    compareWithGolden f = do
        let (name, _) = splitExtension f
        goldenContent <- readFile $ name <.> "golden"
        testContent   <- readFile f
        if goldenContent == testContent
          then pure True
          else do
            failureMessage $ "\nTest " ++ name ++ " failed:"
            False <$ outputFailure testContent goldenContent

    outputFailure :: String -> String -> IO ()
    outputFailure given expected = do
      boldMessage "Expected:"
      putStrLn (importStatementsOnly expected)
      boldMessage "But got:"
      putStrLn (importStatementsOnly given)

successCode, failureCode, resetCode :: [SGR]
successCode = [SetColor Foreground Dull Green]
failureCode = [SetColor Foreground Dull Red]
boldCode = [SetConsoleIntensity BoldIntensity]
resetCode = [Reset]

successMessage, failureMessage :: String -> IO ()
successMessage m = setSGR successCode >> putStrLn m >> setSGR resetCode
failureMessage m = setSGR failureCode >> putStrLn m >> setSGR resetCode
boldMessage m = setSGR boldCode >> putStrLn m >> setSGR resetCode

importStatementsOnly :: String -> String
importStatementsOnly
  = intercalate "\n"
  . map ("- " ++ )
  . filter ("import " `isPrefixOf`)
  . lines
