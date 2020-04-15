module Smuggler.Import where

import Control.Monad (guard)
import qualified Data.HashMap.Strict as HashMap ()
import HsImpExp
  ( IE (IEThingAbs, IEThingAll, IEThingWith, IEVar),
    IEWrappedName (IEName),
    ImportDecl (ideclHiding, ideclName),
    LIE,
    LIEWrappedName,
  )
import HsSyn (GhcRn)
import Name (Name, nameSrcSpan)
import PrelNames (pRELUDE_NAME)
import RnNames (ImportDeclUsage)
import SrcLoc
  ( GenLocated (L),
    SrcSpan (RealSrcSpan, UnhelpfulSpan),
    srcSpanEndCol,
    srcSpanStartCol,
    srcSpanStartLine,
    unLoc,
  )

-- TODO: reuse more logic from GHC. Is it possible?
unusedLocs :: ImportDeclUsage -> [(Int, Int)]
unusedLocs (L (UnhelpfulSpan _) _, _, _) = []
unusedLocs (L (RealSrcSpan loc) decl, used, unused)
  | -- Do not remove `import M ()`
    Just (False, L _ []) <- ideclHiding decl
  = []
  | -- Note [Do not warn about Prelude hiding]
    -- TODO: add ability to support custom prelude
    Just (True, L _ hides) <- ideclHiding decl
  , not (null hides)
  , pRELUDE_NAME == unLoc (ideclName decl)
  = []
  | {-
        This is is not always correct, because instances may be imported
        as in first case above
    
        -- Nothing used; drop entire decl
        | null used = [ (lineNum, colNum)
                      | lineNum <- [srcSpanStartLine loc .. srcSpanEndLine loc]
                      , colNum <-  [srcSpanStartCol loc .. getEndColMax unused]
                      ]
    -}

    -- Everything imported is used; drop nothing
    null unused
  = []
  | -- only part of non-hiding import is used
    Just (False, L _ lies) <- ideclHiding decl
  = unusedLies lies
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
