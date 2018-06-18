module Smuggler.Parser
       ( parseFile
       ) where

import Control.Exception (throwIO)

import Language.Haskell.GHC.ExactPrint (Anns, exactPrint, parseModule)

import HsSyn (HsModule (..))
import RdrName (RdrName)
import SrcLoc (Located)

import Smuggler.Anns (removeAnnAtLoc)
-- import Smuggler.Debug (debugAST)

parseFile :: IO ()
parseFile = do
    let path = "test/input.hs"
    (anns, ast) <- runParser path
    -- debugAST anns
    putStrLn $ exactPrint ast $ removeAnnAtLoc 4 19 anns

runParser :: FilePath -> IO (Anns, Located (HsModule RdrName))
runParser fileName = do
    res <- parseModule fileName
    case res of
        Right x              -> pure x
        Left (_srcSpan, str) -> throwIO $ ParseException str

newtype ParseException = ParseException String
    deriving (Show)

instance Exception ParseException
