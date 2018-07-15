module Smuggler.Plugin
       ( plugin
       ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import HashStore (hashStore)
import Language.Haskell.GHC.ExactPrint (exactPrint)
import System.FilePath (takeFileName)

import HscTypes (ModSummary (..))
import Plugins (CommandLineOption, Plugin (..), defaultPlugin)
import TcRnTypes (TcGblEnv (..), TcM)

import Smuggler.Anns (removeAnnAtLoc)
import Smuggler.Parser (runParser)

import qualified Data.ByteString as BS

plugin :: Plugin
plugin = defaultPlugin { typeCheckResultAction = smugglerPlugin }

cacheDir :: FilePath
cacheDir = ".smuggler"

smugglerPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
smugglerPlugin _ modSummary tcEnv = do
    let modulePath = ms_hspp_file modSummary

    fileContent <- liftIO $ BS.readFile modulePath
    liftIO $ void $ hashStore cacheDir (smuggling modulePath) (takeFileName modulePath, fileContent)  -- TODO: remove takeFileName

    pure tcEnv
  where
    smuggling :: FilePath -> ByteString -> IO ByteString
    smuggling modulePath _ = do
        (anns, ast) <- runParser modulePath  -- TODO: don't read file, use given ByteString
        putStrLn $ exactPrint ast $ removeAnnAtLoc 4 31 anns
        pure ""
