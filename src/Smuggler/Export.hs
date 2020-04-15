module Smuggler.Export where

import Avail (AvailInfo, availNamesWithSelectors)
import Debug.Trace (traceM)
import GHC (AnnKeywordId (..), GenLocated (..), IE (..), IEWrappedName (..), Located, Name)
import Language.Haskell.GHC.ExactPrint.Transform (TransformT, addSimpleAnnT, uniqueSrcSpanT)
import Language.Haskell.GHC.ExactPrint.Types (DeltaPos (DP), GhcPs, KeywordId (G), noExt)
import OccName (HasOccName (occName), OccName (occNameFS))
import RdrName (mkVarUnqual)

-- See https://www.machinesung.com/scribbles/terser-import-declarations.html
-- and https://www.machinesung.com/scribbles/ghc-api.html

mkNamesFromAvailInfos :: [AvailInfo] -> [Name]
mkNamesFromAvailInfos = concatMap availNamesWithSelectors
--Produces all  names made available by the availability information (including overloaded selectors)
--To exclude overloaded selector use availNames

mkIEVarFromNameT :: Monad m => Name -> TransformT m (Located (IE GhcPs))
mkIEVarFromNameT name = do
  -- Could use only one loc as it would be used on different constructors
  -- and not, therefore, get overwritten on subsequent uses.
  locIEVar <- uniqueSrcSpanT
  locIEName <- uniqueSrcSpanT
  locUnqual <- uniqueSrcSpanT
  return $
    L
      locIEVar
      ( IEVar
          noExt
          ( L
              locIEName
              (IEName (L locUnqual (mkVarUnqual ((occNameFS . occName) name))))
          )
      )

addExportDeclAnnT :: Monad m => Located (IE GhcPs) -> TransformT m ()
addExportDeclAnnT (L _ (IEVar _ (L _ (IEName x)))) =
  addSimpleAnnT x (DP (1, 2)) [(G AnnVal, DP (0, 0))]

addCommaT :: Monad m => Located (IE GhcPs) -> TransformT m ()
addCommaT x = addSimpleAnnT x (DP (0, 0)) [(G AnnComma, DP (0, 0))]

addParensT :: Monad m => Located [Located (IE GhcPs)] -> TransformT m ()
addParensT x =
  addSimpleAnnT
    x
    (DP (0, 1))
    [(G AnnOpenP, DP (0, 0)), (G AnnCloseP, DP (0, 1))]
