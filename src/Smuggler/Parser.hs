module Smuggler.Parser
       ( runParser
       ) where

import Control.Exception (throwIO)
import Control.Exception (Exception)
import Language.Haskell.GHC.ExactPrint (Anns, parseModule)

import HsExtension (GhcPs)
import HsSyn (HsModule (..))
import SrcLoc (Located)

-- import Smuggler.Import (getLocationMap)
-- import Smuggler.Name (moduleBodyNames)
-- import Smuggler.Debug (debugAST)

-- parseFile :: IO ()
-- parseFile = do
--     let path = "test/input.hs"
--     (anns, ast@(L _ hsMod)) <- runParser path
--     -- debugAST anns
--     putStrLn $ exactPrint ast $ removeAnnAtLoc 5 22 anns
--
--     -- imports
--     putTextLn "=== Imports map ==="
--     putTextLn $ show $ keys $ getLocationMap hsMod
--
--     putTextLn "=== OccNames ==="
--     putTextLn $ unlines $ map (toText . occNameString) $ moduleBodyNames ast

runParser :: FilePath -> IO (Anns, Located (HsModule GhcPs))
runParser fileName = do
    res <- parseModule fileName
    case res of
        Right x              -> pure x
        Left (_srcSpan, str) -> throwIO $ ParseException str

newtype ParseException = ParseException String
    deriving (Show)

instance Exception ParseException
