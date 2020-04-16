{-# LANGUAGE LambdaCase #-}
module Smuggler.Plugin
  ( plugin
  )
where

import Control.Monad.IO.Class ( liftIO )
import Data.List ( foldl' )
import DynFlags ( DynFlags, HasDynFlags(getDynFlags) )
import GHC.IO.Encoding ( setLocaleEncoding, utf8 )
import HscTypes ( ModSummary(..) )
import HsSyn ( ImportDecl(ideclImplicit), HsModule(hsmodExports) )
import IOEnv ( readMutVar )
import Language.Haskell.GHC.ExactPrint ( exactPrint )
import Plugins
    ( CommandLineOption,
      Plugin(..),
      PluginRecompile(..),
      defaultPlugin )
import RdrName ( GlobalRdrElt )
import RnNames ( findImportUsage )
import Smuggler.Anns ( removeAnnAtLoc, removeTrailingCommas )
import Smuggler.Import ( unusedLocs )
import Smuggler.Export ( addExplicitExports )
import Smuggler.Parser ( runParser )
import SrcLoc ( unLoc, GenLocated(L) )
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

    -- 1. Parse given file
    runParser modulePath fileContents >>= \case
      Left  ()                         -> pure () -- do nothing if file is invalid Haskell
      Right (anns, ast@(L _loc hsMod)) -> do

        -- EXPORTS

        -- What the module exports, implicitly or explicitly
        let allExports             = tcg_exports tcEnv

        let currentExplicitExports = hsmodExports hsMod

        -- 2.  Annotate with exportsListicit export declaration, if there ism't an existing one
        let (anns', ast') = case currentExplicitExports of
              Just _ -> (anns, ast) -- there is an existing export export list
              Nothing ->
                addExplicitExports dflags allExports (anns, ast)

        putStrLn $ exactPrint ast' anns'

        -- IMPORTS

        -- 3. find positions of unused imports
        let user_imports =
              filter (not . ideclImplicit . unLoc) (tcg_rn_imports tcEnv)
        let usage           = findImportUsage user_imports uses
        let unusedPositions = concatMap unusedLocs usage

        -- 4. Remove positions of unused imports from annotations
        case unusedPositions of
          []     -> pure () -- do nothing if no unused imports
          unused -> do
            let purifiedAnnotations = removeTrailingCommas $ foldl'
                  (\ann (x, y) -> removeAnnAtLoc x y ann) -- this seems a bit scattergun
                  anns'
                  unused
            putStrLn $ exactPrint ast' purifiedAnnotations
            let newContent = exactPrint ast' purifiedAnnotations
            case clis of
              []        -> writeFile modulePath newContent
              (ext : _) -> writeFile (modulePath -<.> ext) newContent
