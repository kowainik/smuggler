module Smuggler.Plugin
       ( plugin
       ) where

import Control.Monad.IO.Class (MonadIO (..))
-- import Language.Haskell.GHC.ExactPrint (exactPrint, parseModule)

import DynFlags (getDynFlags)
import FastString (unpackFS)
import HscTypes (HsParsedModule, Hsc, ModSummary (..))
import Outputable (ppr, showSDoc)
import Plugins (CommandLineOption, Plugin (..), defaultPlugin)
import SrcLoc (srcSpanFile)
import TcRnTypes (TcGblEnv (..), TcM)

-- import Smuggler.Anns (removeAnnAtLoc)

plugin :: Plugin
plugin = defaultPlugin
    { renamedResultAction = Just $ \_ _ _ -> pure ()
    , typeCheckResultAction = typecheckPlugin
    }

typecheckPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typecheckPlugin _ ms env = do
--    dflags <- getDynFlags
--     liftIO $ putStrLn $ "rn rn imports: \n" ++ (showSDoc dflags $ ppr $ tcg_rn_imports env)
--
--     liftIO $ putStrLn $ "decls: \n" ++ (showSDoc dflags $ ppr $ tcg_rn_decls env)
--     liftIO $ putStrLn $ "src-span: \n" ++ (showSDoc dflags $ ppr $ srcSpanFile $ tcg_top_loc env)

    liftIO $ putStrLn $ "path: " ++ ms_hspp_file ms

    let modulePath = unpackFS $ srcSpanFile $ tcg_top_loc env
--   (anns, ast@(L _ hsMod)) <- runParser modulePath
    moduleSrc <- liftIO $ readFile modulePath
    liftIO $ putStrLn $ "module source:\n" ++ moduleSrc

    -- debugAST anns
--    putStrLn $ exactPrint ast $ removeAnnAtLoc 4 29 anns

    pure env
