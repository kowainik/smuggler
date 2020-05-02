{-# LANGUAGE LambdaCase #-}
module Smuggler.Plugin
  ( plugin
  )
where

import Control.Monad.IO.Class ( liftIO )
import Data.List ()
import DynFlags ( DynFlags, HasDynFlags(getDynFlags) )
import GHC.IO.Encoding ( setLocaleEncoding, utf8 )
import HscTypes ( ModSummary(..) )
import HsSyn ( ImportDecl(ideclImplicit) )
import IOEnv ( readMutVar )
import Language.Haskell.GHC.ExactPrint ( exactPrint  )
import Language.Haskell.GHC.ExactPrint.Utils ( showAnnData )
import Plugins
    ( CommandLineOption,
      Plugin(..),
      PluginRecompile(..),
      defaultPlugin )
import RdrName ( GlobalRdrElt )
import Smuggler.Import ( minimiseImports )
import Smuggler.Export ( addExplicitExports )
import Smuggler.Options ( parseCommandLineOptions, Options(..) )
import Smuggler.Parser ( runParser )
import SrcLoc ( unLoc )
import System.FilePath ( (-<.>) )
import TcRnTypes ( TcGblEnv(..), TcM )



plugin :: Plugin
plugin = defaultPlugin { typeCheckResultAction = smugglerPlugin
                       , pluginRecompile       = smugglerRecompile
                       }

-- TODO: would it be worth computing a fingerprint to force recompile if
-- imports were removed?
smugglerRecompile :: [CommandLineOption] -> IO PluginRecompile
smugglerRecompile _ = return NoForceRecompile

smugglerPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
smugglerPlugin clis modSummary tcEnv = do
  -- TODO:: Used only for debugging (showSDoc dflags (ppr _ ))
  dflags <- getDynFlags
  let modulePath = ms_hspp_file modSummary
  uses <- readMutVar (tcg_used_gres tcEnv)
  tcEnv <$ liftIO (smuggling dflags uses modulePath)
 where

  smuggling :: DynFlags -> [GlobalRdrElt] -> FilePath -> IO ()
  smuggling dflags uses modulePath = do

    -- 0. Read file content as a UTF-8 string (GHC accepts only ASCII or UTF-8)
    -- TODO: Use ms_hspp_buf instead, if we have it?
    setLocaleEncoding utf8
    fileContents <- readFile modulePath

    let options = parseCommandLineOptions clis

    -- 1. Parse given file
    runParser modulePath fileContents >>= \case
      Left  ()          -> pure () -- do nothing if file is invalid Haskell
      Right (anns, ast) -> do

--        putStrLn $ "showAnnData\n" ++ showAnnData anns 2 ast

        -- 3. Process unused imports
        let user_imports =
              filter (not . ideclImplicit . unLoc) (tcg_rn_imports tcEnv)
        let (anns', ast') =
              minimiseImports dflags (importAction options) user_imports uses
                (anns, ast)

        -- 4. Process exports
        let allExports = tcg_exports tcEnv
        let (anns'', ast'') =
                addExplicitExports dflags (exportAction options) allExports (anns', ast')

--        putStrLn $ "showAnnData\n" ++ showAnnData anns' 2 ast'

        -- 4. Output the result
--        putStrLn $ exactPrint ast' anns'
        let newContent = exactPrint ast'' anns''
        case newExtension options of
          Nothing  -> writeFile modulePath newContent
          Just ext -> writeFile (modulePath -<.> ext) newContent
