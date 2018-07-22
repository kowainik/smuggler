{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

module Smuggler.Plugin
       ( plugin
       ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.List (foldl')
import HashStore (hashStore)
import Language.Haskell.GHC.ExactPrint (exactPrint)
import System.FilePath (takeFileName)

import DynFlags (getDynFlags)
import HscTypes (ModSummary (..))
import HsDoc
import HsExtension (GhcRn)
import HsImpExp (IE (..), IEWrappedName (..), ImportDecl (..), LIE, LIEWrappedName)
import IOEnv (readMutVar)
import Name (Name)
import Outputable
import Plugins (CommandLineOption, Plugin (..), defaultPlugin)
import PrelNames (pRELUDE_NAME)
import RnNames (ImportDeclUsage, findImportUsage)
import SrcLoc (GenLocated (..), SrcSpan (..), getLoc, srcSpanStartCol, srcSpanStartLine, unLoc)
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
    liftIO $ void $ hashStore cacheDir (smuggling uses modulePath) (takeFileName modulePath, fileContent)  -- TODO: remove takeFileName

    pure tcEnv
  where
--    smuggling :: FilePath -> ByteString -> IO ByteString
    smuggling uses modulePath _ = do
        -- 1. find positions of unused imports
        let user_imports = filter (not . ideclImplicit . unLoc) (tcg_rn_imports tcEnv)
        let usage = findImportUsage user_imports uses
        let unusedPositions = concatMap unusedLocs usage
--        debugAST unusedPositions

        -- 2. Remove positions of unused imports from annotations.
        (anns, ast) <- runParser modulePath  -- TODO: don't read file, use given ByteString
        let purifiedAnnotations = foldl' (\ann (x, y) -> removeAnnAtLoc x y ann) anns unusedPositions
        putStrLn $ exactPrint ast purifiedAnnotations

        -- 3. Return empty ByteString
        pure ""

unusedLocs ::ImportDeclUsage -> [(Int, Int)]
unusedLocs (L (RealSrcSpan loc) decl, used, unused)
    -- Do not warn for `import M ()`
    | Just (False, L _ []) <- ideclHiding decl
    = []

    -- Note [Do not warn about Prelude hiding]
    -- TODO: add ability to support custom prelude
    | Just (True, L _ hides) <- ideclHiding decl
    , not (null hides)
    , pRELUDE_NAME == unLoc (ideclName decl)
    = []

    -- Nothing used; drop entire decl
    -- TODO: drop every line of multiline import
    -- TODO: optimize
    | null used = map (srcSpanStartLine loc,) [1..100]

    -- Everything imported is used; drop nothing
    | null unused = []

    -- only part of non-hiding import is used
    | Just (False, L _ lies) <- ideclHiding decl
    = unusedEntries lies

    -- TODO: unused hidings
    | otherwise = []
  where
    unusedEntries :: [LIE GhcRn] -> [(Int, Int)]
    unusedEntries = concatMap lieToLoc

    lieToLoc :: LIE GhcRn -> [(Int, Int)]
    lieToLoc (L _ lie) = case lie of
        IEVar _ name                 -> lieNameToLoc name
        IEThingAbs _ name            -> lieNameToLoc name
        IEThingAll _ name            -> lieNameToLoc name
        IEThingWith _ name _ names _ -> concatMap lieNameToLoc (name : names)
        _                            -> []

    lieNameToLoc :: LIEWrappedName Name -> [(Int, Int)]
    lieNameToLoc (unLoc -> IEName (L (RealSrcSpan lieLoc) name)) =
        if name `elem` unused then [(srcSpanStartLine lieLoc, srcSpanStartCol lieLoc)] else []
    lieNameToLoc _ = []
