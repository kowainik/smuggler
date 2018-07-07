module Smuggler.Plugin
       ( plugin
       ) where

import Control.Monad.IO.Class (MonadIO (..))
import Language.Haskell.GHC.ExactPrint (exactPrint)

import HscTypes (ModSummary (..))
import Plugins (CommandLineOption, Plugin (..), defaultPlugin)
import TcRnTypes (TcGblEnv (..), TcM)

import Smuggler.Anns (removeAnnAtLoc)
import Smuggler.Parser (runParser)

plugin :: Plugin
plugin = defaultPlugin { typeCheckResultAction = smugglerPlugin }

smugglerPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
smugglerPlugin _ modSummary tcEnv = do
    let modulePath = ms_hspp_file modSummary

    (anns, ast) <- liftIO $ runParser modulePath
    liftIO $ putStrLn $ exactPrint ast $ removeAnnAtLoc 4 31 anns

    pure tcEnv
