module Smuggler.Import where

import Control.Monad ( guard )
import HsSyn ( GhcRn )
import Name ( Name, nameSrcSpan )
import PrelNames ( pRELUDE_NAME )
import RnNames ( ImportDeclUsage, findImportUsage )
import Smuggler.Options ( ImportAction(..) )
import Smuggler.Anns ( removeAnnAtLoc, removeTrailingCommas )
import Data.List ( foldl' )
import DynFlags ( DynFlags )
import RdrName ( GlobalRdrElt )
import HsImpExp
    ( IE(IEThingWith, IEVar, IEThingAbs, IEThingAll),
      IEWrappedName(IEName),
      ImportDecl(ideclName, ideclHiding),
      LIE,
      LIEWrappedName,
      LImportDecl )
import Language.Haskell.GHC.ExactPrint.Types ( Anns )
import SrcLoc
    ( SrcSpan(..),
      Located,
      srcSpanEndCol,
      srcSpanEndLine,
      srcSpanStartCol,
      srcSpanStartLine,
      unLoc,
      GenLocated(L) )
import GHC ( HsModule, GhcPs )

minimiseImports
  :: DynFlags
  -> ImportAction
  -> [LImportDecl GhcRn]
  -> [GlobalRdrElt]
  -> (Anns, Located (HsModule GhcPs))
  -> (Anns, Located (HsModule GhcPs))
minimiseImports dflags action user_imports uses p@(anns, ast) = case action of
  NoImportProcessing -> p
  _ -> if null unusedPositions then p else (purifiedAnnotations, ast)
 where
  usage :: [ImportDeclUsage]
  usage = findImportUsage user_imports uses

  unusedPositions :: [(Int, Int)]
  unusedPositions = concatMap (unusedLocs action) usage

  purifiedAnnotations :: Anns
  purifiedAnnotations = removeTrailingCommas -- Does removeTrailing commas work on []? If so simplify above
    $ foldl' (\ann (x, y) -> removeAnnAtLoc x y ann) -- this seems a bit scattergun
                                                     anns unusedPositions

-- TODO: rewrite this as a transform, like Export

-- TODO: reuse more logic from GHC. Is it possible?
unusedLocs :: ImportAction -> ImportDeclUsage -> [(Int, Int)]
unusedLocs _ (L (UnhelpfulSpan _) _, _, _) = []
unusedLocs action (L (RealSrcSpan loc) decl, used, unused)
  | -- Do not remove `import M ()`
    Just (False, L _ []) <- ideclHiding decl
  = []

  | -- Note [Do not warn about Prelude hiding]
    -- TODO: add ability to support custom prelude
    Just (True, L _ hides) <- ideclHiding decl
  , not (null hides)
  , pRELUDE_NAME == unLoc (ideclName decl)
  = []

  | -- only part of non-hiding import is used
    Just (False, L _ lies) <- ideclHiding decl
  = unusedLies lies

  | -- Nothing used
    null used
  = case action of
    PreserveInstanceImports ->
      case ideclHiding decl of
        Nothing -> [] -- ?
        Just (True, _) -> [] -- TODO: deal with unused hidings
        Just (False, L _ lies) -> unusedLies lies
    MinimiseImports ->
      -- Drop entire decl
      [ (lineNum, colNum)
      | lineNum <- [srcSpanStartLine loc .. srcSpanEndLine loc]
      , colNum  <- [srcSpanStartCol loc .. getEndColMax unused]
      ]
    NoImportProcessing -> error "Processing imports, when should not be doing so"

  | -- Everything imported is used; drop nothing
    null unused
  = []

  | -- TODO: unused hidings
    otherwise
  = []
 where
  unusedLies :: [LIE GhcRn] -> [(Int, Int)]
  unusedLies = concatMap lieToLoc

  lieToLoc :: LIE GhcRn -> [(Int, Int)]
  lieToLoc (L _ lie) = case lie of
    IEVar      _ name            -> lieNameToLoc name
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
  findColLoc (RealSrcSpan   l) = srcSpanEndCol l
  findColLoc (UnhelpfulSpan _) = defaultCol

defaultCol :: Int
defaultCol = 120

listMax :: [Int] -> Int
listMax []           = defaultCol
listMax [x         ] = x
listMax (x : y : xs) = listMax ((if x >= y then x else y) : xs)
