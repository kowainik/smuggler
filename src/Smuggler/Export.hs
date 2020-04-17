module Smuggler.Export (addExplicitExports) where


import Avail ( AvailInfo, availNamesWithSelectors )
import Control.Monad ( unless )
import Data.Maybe ( isNothing )
import DynFlags ( DynFlags )
import GHC
    ( AnnKeywordId(..),
      GenLocated(..),
      HsModule,
      IE(..),
      IEWrappedName(..),
      Located,
      Name,
      hsmodExports )
import Language.Haskell.GHC.ExactPrint.Transform
    ( TransformT, addSimpleAnnT, runTransform, uniqueSrcSpanT )
import Language.Haskell.GHC.ExactPrint.Types
    ( Anns, DeltaPos(DP), GhcPs, KeywordId(G), noExt )
import OccName ( HasOccName(occName), OccName(occNameFS) )
import RdrName ( mkVarUnqual )
import Smuggler.Options ( ExportAction(..) )

-- See https://www.machinesung.com/scribbles/terser-import-declarations.html
-- and https://www.machinesung.com/scribbles/ghc-api.html


addExplicitExports
  :: DynFlags
  -> ExportAction
  -> [AvailInfo]
  -> (Anns, Located (HsModule GhcPs))
  -> (Anns, Located (HsModule GhcPs))
addExplicitExports dflags action exports p@(anns, L astLoc hsMod) =
  case action of
    NoExportProcessing -> p
    AddExplicitExports ->
      if isNothing currentExplicitExports then (anns', ast') else p
    ReplaceExports -> (anns', ast')
 where
  currentExplicitExports  = hsmodExports hsMod

  (ast', (anns', _n), _s) = runTransform anns $ do

    let names = mkNamesFromAvailInfos exports

    exportsList <- mapM mkIEVarFromNameT names
    mapM_ addExportDeclAnnT exportsList
    unless (null exportsList) $ mapM_ addCommaT (init exportsList)

    let lExportsList = L astLoc exportsList
        hsMod'       = hsMod { hsmodExports = Just lExportsList }
    addParensT lExportsList

    return (L astLoc hsMod')


mkNamesFromAvailInfos :: [AvailInfo] -> [Name]
mkNamesFromAvailInfos = concatMap availNamesWithSelectors
--Produces all  names made available by the availability information (including overloaded selectors)
--To exclude overloaded selector use availNames

mkIEVarFromNameT :: Monad m => Name -> TransformT m (Located (IE GhcPs))
mkIEVarFromNameT name = do
  -- Could use only one loc as it would be used on different constructors
  -- and not, therefore, get overwritten on subsequent uses.
  locIEVar  <- uniqueSrcSpanT
  locIEName <- uniqueSrcSpanT
  locUnqual <- uniqueSrcSpanT
  return $ L
    locIEVar
    (IEVar
      noExt
      (L locIEName
         (IEName (L locUnqual (mkVarUnqual ((occNameFS . occName) name))))
      )
    )

addExportDeclAnnT :: Monad m => Located (IE GhcPs) -> TransformT m ()
addExportDeclAnnT (L _ (IEVar _ (L _ (IEName x)))) =
  addSimpleAnnT x (DP (1, 2)) [(G AnnVal, DP (0, 0))]

addCommaT :: Monad m => Located (IE GhcPs) -> TransformT m ()
addCommaT x = addSimpleAnnT x (DP (0, 0)) [(G AnnComma, DP (0, 0))]

addParensT :: Monad m => Located [Located (IE GhcPs)] -> TransformT m ()
addParensT x = addSimpleAnnT
  x
  (DP (0, 1))
  [(G AnnOpenP, DP (0, 0)), (G AnnCloseP, DP (0, 1))]
