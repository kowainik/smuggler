{-# LANGUAGE TupleSections #-}

module Smuggler.Plugin
       ( plugin
       ) where

import Control.Monad (guard, void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.List (foldl')
import HashStore (hashStore)
import Language.Haskell.GHC.ExactPrint (exactPrint)
import System.FilePath ((-<.>))

import HscTypes (ModSummary (..))
import HsExtension (GhcRn)
import HsImpExp (IE (..), IEWrappedName (..), ImportDecl (..), LIE, LIEWrappedName)
import IOEnv (readMutVar)
import Name (Name)
import Plugins (CommandLineOption, Plugin (..), defaultPlugin)
import PrelNames (pRELUDE_NAME)
import RdrName (GlobalRdrElt)
import RnNames (ImportDeclUsage, findImportUsage)
import SrcLoc (GenLocated (..), SrcSpan (..), srcSpanStartCol, srcSpanStartLine, unLoc)
import TcRnTypes (TcGblEnv (..), TcM)

import Smuggler.Anns (removeAnnAtLoc)
import Smuggler.Parser (runParser)

import qualified Data.ByteString as BS

plugin :: Plugin
plugin = defaultPlugin { typeCheckResultAction = smugglerPlugin }

cacheDir :: FilePath
cacheDir = ".smuggler"

munglePath :: FilePath -> FilePath
munglePath = \case
    []       -> []
    '/' : xs -> '.' : munglePath xs
    c   : xs -> c   : munglePath xs

smugglerPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
smugglerPlugin clis modSummary tcEnv = do
    let modulePath = ms_hspp_file modSummary

    uses <- readMutVar (tcg_used_gres tcEnv)
    fileContent <- liftIO $ BS.readFile modulePath
    let modifiedName = munglePath modulePath

    liftIO $ void $ hashStore cacheDir
                              (smuggling uses modulePath)
                              (modifiedName, fileContent)

    pure tcEnv
  where
    smuggling :: [GlobalRdrElt] -> FilePath -> ByteString -> IO ByteString
    smuggling uses modulePath _ =  do
        -- 1. Parse given file
        runParser modulePath >>= \case -- TODO: don't read file, use given ByteString
            Left () -> pure ()  -- do nothing if file is invalid Haskell
            Right (anns, ast) -> do
                -- 2. find positions of unused imports
                let user_imports = filter (not . ideclImplicit . unLoc) (tcg_rn_imports tcEnv)
                let usage = findImportUsage user_imports uses
                let unusedPositions = concatMap unusedLocs usage

                -- 3. Remove positions of unused imports from annotations.
                let purifiedAnnotations = foldl' (\ann (x, y) -> removeAnnAtLoc x y ann) anns unusedPositions
                let newContent = exactPrint ast purifiedAnnotations
                case clis of
                    []      -> putStrLn newContent
                    (ext:_) -> writeFile (modulePath -<.> ext) newContent
        -- 4. Return empty ByteString
        pure ""

unusedLocs ::ImportDeclUsage -> [(Int, Int)]
unusedLocs (L (UnhelpfulSpan _) _, _, _) = []
unusedLocs (L (RealSrcSpan loc) decl, used, unused)
    -- Do not remove `import M ()`
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
    = unusedLies lies

    -- TODO: unused hidings
    | otherwise = []
  where
    unusedLies :: [LIE GhcRn] -> [(Int, Int)]
    unusedLies = concatMap lieToLoc

    lieToLoc :: LIE GhcRn -> [(Int, Int)]
    lieToLoc (L _ lie) = case lie of
        IEVar _ name                 -> lieNameToLoc name
        IEThingAbs _ name            -> lieNameToLoc name
        IEThingAll _ name            -> lieNameToLoc name
        IEThingWith _ name _ names _ -> concatMap lieNameToLoc (name : names)
        _                            -> []

    lieNameToLoc :: LIEWrappedName Name -> [(Int, Int)]
    lieNameToLoc lieName = do
        L _ (IEName (L (RealSrcSpan lieLoc) name)) <- [lieName]
        guard $ name `elem` unused
        pure (srcSpanStartLine lieLoc, srcSpanStartCol lieLoc)
