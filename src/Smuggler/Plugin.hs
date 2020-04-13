module Smuggler.Plugin
  ( plugin
  )
where

import           Control.Monad                  ( guard )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.List                      ( foldl' )
import           GHC.IO.Encoding                ( setLocaleEncoding
                                                , utf8
                                                )
import           HscTypes                       ( ModSummary(..) )
import           HsExtension                    ( GhcRn )
import           HsImpExp                       ( IE(..)
                                                , IEWrappedName(..)
                                                , ImportDecl(..)
                                                , LIE
                                                , LIEWrappedName
                                                )
import           IOEnv                          ( readMutVar )
import           Language.Haskell.GHC.ExactPrint
                                                ( exactPrint
                                                , Anns (..)
                                                , uniqueSrcSpanT
                                                , addAnnotationsForPretty
                                                , addSimpleAnnT
                                                , runTransform )
import           Name                           ( Name
                                                , nameSrcSpan
                                                )
import           Plugins                        ( CommandLineOption
                                                , Plugin(..)
                                                , PluginRecompile(..)
                                                , defaultPlugin
                                                )
import           PrelNames                      ( pRELUDE_NAME )
import           RdrName                        ( GlobalRdrElt , isLocalGRE, gresToAvailInfo)
import           RnNames                        ( ImportDeclUsage
                                                , findImportUsage
                                                )
import           Smuggler.Anns                  ( removeAnnAtLoc
                                                , removeTrailingCommas
                                                )
import           Smuggler.Parser                ( runParser )
import           SrcLoc                         ( GenLocated(L)
                                                , noLoc
                                                , Located
                                                , SrcSpan(..)
                                                , srcSpanEndCol
                                                , srcSpanStartCol
                                                , srcSpanStartLine
                                                , unLoc
                                                )
import           System.FilePath                ( (-<.>) )
import           TcRnTypes                      ( TcGblEnv(..)
                                                , TcM
                                                )


import ApiAnnotation
import DynFlags
import Outputable
import TcRnExports
import Language.Haskell.GHC.ExactPrint.Types (DeltaPos( DP ) ,  KeywordId( G ))
import Language.Haskell.GHC.ExactPrint.Utils (showAnnData)
import           Language.Haskell.GHC.ExactPrint.Transform
import Avail
import HsSyn
import Smuggler.Exports
import Text.Pretty.Simple
import Debug.Trace

plugin :: Plugin
plugin = defaultPlugin { typeCheckResultAction = smugglerPlugin,
                         pluginRecompile = smugglerRecompile }

defaultCol :: Int
defaultCol = 120

-- TODO: would it be worth computing a fingerprint to force recompile if
-- imports were removed?
smugglerRecompile :: [CommandLineOption] -> IO PluginRecompile
smugglerRecompile _ = return NoForceRecompile

smugglerPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
smugglerPlugin clis modSummary tcEnv = do
    dflags <- getDynFlags
    let modulePath = ms_hspp_file modSummary
    uses <- readMutVar (tcg_used_gres tcEnv)
    tcEnv <$ liftIO (smuggling dflags uses modulePath)
  where

    addExports
      :: DynFlags
      -> [AvailInfo]
      -> (Anns, Located (HsModule GhcPs))
      -> (Anns, Located (HsModule GhcPs))
    addExports dflags exports (anns, ast@(L astLoc hsMod)) = do
      let
        names = mkNamesFromAvailInfos exports
        (exports', (anns', n), s) =
          -- runTransform :: Anns -> Transform a -> (a, (Anns, Int), [String])
          runTransform anns $ mapM mkIEVarFromNameT names

        addExportDecls
          :: [Located (IE GhcPs)] -> Transform (Located (HsModule GhcPs))
        addExportDecls expl = do
          let hsMod' = hsMod { hsmodExports = Just $ L astLoc expl }
          addSimpleAnnT (L astLoc expl)
                        (DP (0, 1))
                        [(G AnnOpenP, DP (0, 0)), (G AnnCloseP, DP (0, 1))]
          mapM_ addExportDeclAnnT expl
          mapM_ addCommaT (init expl)

          traceM $ "astLoc: " ++ show astLoc

          return (L astLoc hsMod')

        (ast', (anns'', n'), s') = runTransform anns' (addExportDecls exports')

      
      (anns'', ast')

    smuggling :: DynFlags -> [GlobalRdrElt] -> FilePath -> IO ()
    smuggling dflags uses modulePath = do
        -- 0. Read file content as a UTF-8 string (GHC accepts only ASCII or UTF-8)
        -- TODO: Use ms_hspp_buf instead, if we have it?
        setLocaleEncoding utf8
        fileContents <- readFile modulePath

        -- 1. Parse given file
        runParser modulePath fileContents >>= \case
            Left () -> pure ()  -- do nothing if file is invalid Haskell
            Right (anns, ast@(L astLoc hsMod)) -> do
              
                let allExports = tcg_exports tcEnv
                putStrLn  $ "AllExports:" ++ showSDoc dflags (ppr allExports)

                -- hsmodExports :: Maybe (Located [LIE pass])
                let currentExplicitExports = hsmodExports hsMod 
                putStrLn  $ "currentExplicitExports:" ++ showSDoc dflags (ppr currentExplicitExports)

                case currentExplicitExports of
                 Just _ -> putStrLn "Leave alone"
                 Nothing -> do
                              let (anns', ast') = addExports dflags allExports (anns, ast)
                              putStrLn  $ "ast':" ++ showSDoc dflags (ppr ast')
--                              putStrLn  $ "anns':" ++  showSDoc dflags (ppr anns')
                              putStrLn $ "exactPrint:\n" ++ exactPrint ast'  anns'
                              --putStrLn $ exactPrint ast' (addAnnotationsForPretty [] ast' anns')

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

{-
    This is is not always correct, because instances may be imported
    as in first case above

    -- Nothing used; drop entire decl
    | null used = [ (lineNum, colNum)
                  | lineNum <- [srcSpanStartLine loc .. srcSpanEndLine loc]
                  , colNum <-  [srcSpanStartCol loc .. getEndColMax unused]
                  ]
-}

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
    findColLoc (RealSrcSpan l)   = srcSpanEndCol l
    findColLoc (UnhelpfulSpan _) = defaultCol

listMax :: [Int] -> Int
listMax []       = defaultCol
listMax [x]      = x
listMax (x:y:xs) = listMax ((if x >= y then x else y):xs)
