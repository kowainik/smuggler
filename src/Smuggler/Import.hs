module Smuggler.Import where

import           Control.Monad                  ( guard )
import           HsSyn                          ( GhcRn )
import           Name                           ( Name
                                                , nameSrcSpan
                                                )
import           PrelNames                      ( pRELUDE_NAME )
import           RnNames                        ( ImportDeclUsage
                                                , findImportUsage
                                                , getMinimalImports
                                                )
import           Smuggler.Options               ( ImportAction(..) )
import           Smuggler.Anns                  ( removeAnnAtLoc
                                                , removeTrailingCommas
                                                )
import           Data.List                      ( foldl' )
import           DynFlags                       ( DynFlags )
import           RdrName                        ( GlobalRdrElt )
import           HsImpExp                       ( ideclImplicit
                                                , ieLWrappedName
                                                , IE
                                                  ( IEThingWith
                                                  , IEVar
                                                  , IEThingAbs
                                                  , IEThingAll
                                                  )
                                                , IEWrappedName(IEName)
                                                , ImportDecl
                                                  ( ideclName
                                                  , ideclHiding
                                                  )
                                                , LIE
                                                , LIEWrappedName
                                                , LImportDecl
                                                )
import           Language.Haskell.GHC.ExactPrint.Types
                                                ( Anns )
import           SrcLoc                         ( SrcSpan(..)
                                                , Located
                                                , srcSpanEndCol
                                                , srcSpanEndLine
                                                , srcSpanStartCol
                                                , srcSpanStartLine
                                                , unLoc
                                                , GenLocated(L)
                                                )
import           GHC                            ( AnnKeywordId(..)
                                                , HsModule
                                                , GhcPs
                                                , hsmodImports
                                                )
import           Language.Haskell.GHC.ExactPrint.Transform
                                                ( TransformT
                                                , addSimpleAnnT
                                                , runTransform
                                                , uniqueSrcSpanT
                                                )
import           Outputable
import           TcRnTypes
import           Avail
import           LoadIface
import           BasicTypes
import           Language.Haskell.GHC.ExactPrint.Utils
                                                ( ss2posEnd, debug )
import           Language.Haskell.GHC.ExactPrint.Types
                                                ( Anns
                                                , DeltaPos(DP)
                                                , KeywordId(G)
                                                , noExt
                                                )
import Language.Haskell.GHC.ExactPrint.Print (exactPrint)


minimiseImports
  :: DynFlags
  -> ImportAction
  -> [LImportDecl GhcRn]
  -> [GlobalRdrElt]
  -> (Anns, Located (HsModule GhcPs))
  -> (Anns, Located (HsModule GhcPs))
minimiseImports dflags action user_imports uses p@(anns, ast) = case action of
  NoImportProcessing -> p
  _                  -> (purifiedAnnotations, ast)
 where
  usage :: [ImportDeclUsage]
  usage                    = findImportUsage user_imports uses

  (anns', unusedPositions) = findUnusedLocs anns usage

  findUnusedLocs :: Anns -> [ImportDeclUsage] -> (Anns, [(Int, Int)])
  findUnusedLocs anns []       = (anns, [])
  findUnusedLocs anns (u : us) = (anns'', unusedPositions ++ unusedPositions')
   where
    (anns' , unusedPositions ) = unusedLocs dflags action anns u
    (anns'', unusedPositions') = findUnusedLocs anns' us


  purifiedAnnotations :: Anns
  purifiedAnnotations = removeTrailingCommas -- Does removeTrailing commas work on []? If so simplify above
    $ foldl' (\ann (x, y) -> removeAnnAtLoc x y ann) -- this seems a bit scattergun
                                                     anns' unusedPositions

-- TODO: rewrite this as a transform, like Export?

-- TODO: reuse more logic from GHC. Is it possible?
unusedLocs
  :: DynFlags -> ImportAction -> Anns -> ImportDeclUsage -> (Anns, [(Int, Int)])
unusedLocs _ _ anns (L (UnhelpfulSpan _) _, _, _) = (anns, [])
unusedLocs dynflags action anns (imp@(L (RealSrcSpan loc) decl), used, unused)
  | -- Do not remove `import M ()`
    Just (False, L _ []) <- ideclHiding decl
  = (anns, [])
  | -- Note [Do not warn about Prelude hiding]
    -- TODO: add ability to support custom prelude
    Just (True, L _ hides) <- ideclHiding decl
  , not (null hides)
  , ideclImplicit decl -- pRELUDE_NAME == unLoc (ideclName decl)
  = (anns, [])
  | -- Nothing used
    null used
  = case action of
    PreserveInstanceImports -> case ideclHiding decl of
--      Nothing -> trace ("Nothing:\ndecl: " ++ showSDoc dynflags (ppr decl)
--           ++ " used: " ++ showSDoc dynflags (ppr used)
--           ++ " unused: " ++ showSDoc dynflags (ppr unused)
--           ) []
      Nothing -> 

        -- This isn't enough;  need also to mod iDeclHiding decl
        let (_ast, (anns', _n), _s) = runTransform anns $ do
              addSimpleAnnT
                imp
                (DP (0, 0))
                [(G AnnOpenP,DP (0,0)),(G AnnCloseP,DP (0,0))]

        in  (anns', []) 


      Just (True , _       ) -> (anns, []) -- TODO: deal with unused hidings
      Just (False, L _ lies) -> (anns, unusedLies lies)
    MinimiseImports ->
      ( anns
      ,
      -- Drop entire decl
        [ (lineNum, colNum)
        | lineNum <- [srcSpanStartLine loc .. srcSpanEndLine loc]
        , colNum <- [srcSpanStartCol loc .. getEndColMax unused]
        ]
      )
    NoImportProcessing ->
      error "Processing imports, when should not be doing so"
  | null unused
  = (anns, [])
  | -- Everything imported is used; drop nothing -- only part of non-hiding import is used
    Just (False, L _ lies) <- ideclHiding decl
  = (anns, unusedLies lies)
  | -- TODO: unused hidings
    otherwise
  = (anns, [])
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
    L lieLoc name <- [ieLWrappedName lieName]
    guard $ name `elem` unused
    pure (ss2posEnd lieLoc)

  getEndColMax :: [Name] -> Int
  getEndColMax u = listMax $ map (srcSpanEndColumn' . nameSrcSpan) u

  srcSpanEndColumn' :: SrcSpan -> Int
  srcSpanEndColumn' (RealSrcSpan   l) = srcSpanEndCol l
  srcSpanEndColumn' (UnhelpfulSpan _) = defaultCol

defaultCol :: Int
defaultCol = 120

listMax :: [Int] -> Int
listMax []           = defaultCol
listMax [x         ] = x
listMax (x : y : xs) = listMax ((if x >= y then x else y) : xs)




removeSurplusImports
  :: DynFlags
  -> ImportAction
  -> [LImportDecl GhcRn]
  -> [GlobalRdrElt]
  -> (Anns, Located (HsModule GhcPs))
  -> (Anns, Located (HsModule GhcPs))
removeSurplusImports dflags action user_imports uses p@(anns, L astLoc hsMod) =
  case action of
    NoImportProcessing -> p
    _                  -> (anns', ast')
 where
  usage :: [ImportDeclUsage] -- [(LImportDecl GhcRn, used: [AvailInfo], unused: [Name])]
  usage = findImportUsage user_imports uses

  minimalUsage :: RnM [LImportDecl GhcRn]
  minimalUsage            = getMinimalImports usage

     --  - 'ApiAnnotation.AnnOpen', 'ApiAnnotation.AnnClose' for ideclSource

     --  - 'ApiAnnotation.AnnHiding','ApiAnnotation.AnnOpen',
     --    'ApiAnnotation.AnnClose' attached
     --     to location in ideclHiding

  (ast', (anns', _n), _s) = runTransform anns $ do

    let hsMod' = hsMod { hsmodImports = hsmodImports hsMod }
    return (L astLoc hsMod')


