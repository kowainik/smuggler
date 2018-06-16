module Smuggler.Parser
       ( parseFile
       ) where

import Control.Exception (throwIO)

import Language.Haskell.GHC.ExactPrint (Anns, exactPrint, parseModule)

import HsSyn (HsModule (..))
import RdrName (RdrName)
import SrcLoc (Located)

parseFile :: IO ()
parseFile = do
    let path = "src/Smuggler/Parser.hs"
    (anns, ast) <- runParser path
    putStrLn $ exactPrint ast anns

runParser :: FilePath -> IO (Anns, Located (HsModule RdrName))
runParser fileName = do
    res <- parseModule fileName
    case res of
        Right x              -> pure x
        Left (_srcSpan, str) -> throwIO $ ParseException str

newtype ParseException = ParseException String
    deriving (Show)

instance Exception ParseException
