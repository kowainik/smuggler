module Smuggler.Plugin
       ( plugin
       ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import HashStore (hashStore)
import Language.Haskell.GHC.ExactPrint (exactPrint)
import System.FilePath (takeFileName)

import DynFlags (getDynFlags)
import HscTypes (ModSummary (..))
import HsDoc
import HsImpExp (ImportDecl (..))
import IOEnv (readMutVar)
import Outputable
import Plugins (CommandLineOption, Plugin (..), defaultPlugin)
import RnNames (findImportUsage)
import SrcLoc (getLoc, unLoc)
import TcRnTypes (TcGblEnv (..), TcM)

import Smuggler.Anns (removeAnnAtLoc)
import Smuggler.Debug (debugAST)
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
    uses <- readMutVar (tcg_used_gres tcEnv)
    dflags <- getDynFlags
    liftIO $ void $ hashStore cacheDir (smuggling dflags uses modulePath) (takeFileName modulePath, fileContent)  -- TODO: remove takeFileName

    pure tcEnv
  where
--    smuggling :: FilePath -> ByteString -> IO ByteString
    smuggling dflags uses modulePath _ = do
        let user_imports = filter (not . ideclImplicit . unLoc) (tcg_rn_imports tcEnv)
        let usageLocs = map (\(_,info,_) -> info) $ findImportUsage user_imports uses
        mapM_ (putStrLn . showSDoc dflags . ppr) usageLocs

        (anns, ast) <- runParser modulePath  -- TODO: don't read file, use given ByteString
        putStrLn $ exactPrint ast $ removeAnnAtLoc 4 31 anns
        pure ""
