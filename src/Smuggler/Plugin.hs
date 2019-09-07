module Smuggler.Plugin
       ( plugin
       ) where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (foldl')
import Language.Haskell.GHC.ExactPrint (exactPrint)
import System.FilePath ((-<.>))

import HscTypes (ModSummary (..))
import HsExtension (GhcRn)
import HsImpExp (IE (..), IEWrappedName (..), ImportDecl (..), LIE, LIEWrappedName)
import IOEnv (readMutVar)
import Name (Name, nameSrcSpan)
import Plugins (CommandLineOption, Plugin (..), defaultPlugin)
import PrelNames (pRELUDE_NAME)
import RdrName (GlobalRdrElt)
import RnNames (ImportDeclUsage, findImportUsage)
import SrcLoc (GenLocated (..), SrcSpan (..), srcSpanEndCol, srcSpanEndLine, srcSpanStartCol,
               srcSpanStartLine, unLoc)
import TcRnTypes (TcGblEnv (..), TcM)

import Smuggler.Anns (removeAnnAtLoc, removeTrailingCommas)
import Smuggler.Parser (runParser)

import qualified Data.ByteString as BS


plugin :: Plugin
plugin = defaultPlugin { typeCheckResultAction = smugglerPlugin }

defaultCol :: Int
defaultCol = 120

smugglerPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
smugglerPlugin clis modSummary tcEnv = do
    let modulePath = ms_hspp_file modSummary
    uses <- readMutVar (tcg_used_gres tcEnv)
    tcEnv <$ liftIO (smuggling uses modulePath)
  where
    smuggling :: [GlobalRdrElt] -> FilePath -> IO ()
    smuggling uses modulePath = do
        -- 0. Read file content as a raw ByteString
        fileContents <- BS.readFile modulePath

        -- 1. Parse given file
        runParser modulePath fileContents >>= \case
            Left () -> pure ()  -- do nothing if file is invalid Haskell
            Right (anns, ast) -> do
                -- 2. find positions of unused imports
                let user_imports = filter (not . ideclImplicit . unLoc) (tcg_rn_imports tcEnv)
                let usage = findImportUsage user_imports uses
                let unusedPositions = concatMap unusedLocs usage

                -- 3. Remove positions of unused imports from annotations
                case unusedPositions of
                    [] -> pure ()  -- do nothing if no unused imports
                    unused -> do
                        let purifiedAnnotations = removeTrailingCommas
                                $ foldl' (\ann (x, y) -> removeAnnAtLoc x y ann) anns unused
                        let newContent = exactPrint ast purifiedAnnotations
                        case clis of
                            []      -> writeFile modulePath newContent
                            (ext:_) -> writeFile (modulePath -<.> ext) newContent

-- TODO: reuse more logic from GHC. Is it possible?
unusedLocs :: ImportDeclUsage -> [(Int, Int)]
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
    | null used = [ (lineNum, colNum)
                  | lineNum <- [srcSpanStartLine loc .. srcSpanEndLine loc]
                  , colNum <-  [srcSpanStartCol loc .. getEndColMax unused]
                  ]

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

    getEndColMax :: [Name] -> Int
    getEndColMax u = listMax $ map (findColLoc . nameSrcSpan) u

    findColLoc :: SrcSpan -> Int
    findColLoc (RealSrcSpan l)   =  srcSpanEndCol l
    findColLoc (UnhelpfulSpan _) = defaultCol

listMax :: [Int] -> Int
listMax []       = defaultCol
listMax [x]      = x
listMax (x:y:xs) = listMax ((if x >= y then x else y):xs)
