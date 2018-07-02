module Smuggler.Plugin
       ( plugin
       ) where

import Avail
import Control.Monad.IO.Class
import DynFlags (getDynFlags)
import HscTypes
import HsDecls
import HsDoc
import HsExpr
import HsExtension
import HsImpExp
import Outputable
import Plugins
import TcRnTypes

plugin :: Plugin
plugin = defaultPlugin { parsedResultAction = parsedPlugin
                       , renamedResultAction = Just renamedAction
                       , typeCheckResultAction = typecheckPlugin
                       , spliceRunAction = metaPlugin
                       , interfaceLoadAction = interfaceLoadPlugin
                       }

parsedPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedPlugin _ _ pm
  = do dflags <- getDynFlags
       liftIO $ putStrLn $ "parsePlugin: \n" ++ (showSDoc dflags $ ppr $ hpm_module pm)
       return pm

renamedAction :: [CommandLineOption] -> ModSummary
                    -> ( HsGroup GhcRn, [LImportDecl GhcRn]
                       , Maybe [(LIE GhcRn, Avails)], Maybe LHsDocString )
                    -> TcM ()
renamedAction _ _ ( gr, _, _, _ )
  = do dflags <- getDynFlags
       liftIO $ putStrLn $ "typeCheckPlugin (rn): " ++ (showSDoc dflags $ ppr gr)

typecheckPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typecheckPlugin _ _ tc
  = do dflags <- getDynFlags
       liftIO $ putStrLn $ "typeCheckPlugin (rn): \n" ++ (showSDoc dflags $ ppr $ tcg_rn_decls tc)
       liftIO $ putStrLn $ "typeCheckPlugin (tc): \n" ++ (showSDoc dflags $ ppr $ tcg_binds tc)
       return tc

metaPlugin :: [CommandLineOption] -> LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
metaPlugin _ meta
  = do dflags <- getDynFlags
       liftIO $ putStrLn $ "meta: " ++ (showSDoc dflags $ ppr meta)
       return meta

interfaceLoadPlugin :: [CommandLineOption] -> ModIface -> IfM lcl ModIface
interfaceLoadPlugin _ iface
  = do dflags <- getDynFlags
       liftIO $ putStrLn $ "interface loaded: " ++ (showSDoc dflags $ ppr $ mi_module iface)
       return iface
