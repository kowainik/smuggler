module Smuggler.Exports where

import           GHC                            ( AnnKeywordId(..)
                                                , IEWrappedName(..)
                                                , Name
                                                , Located
                                                , GenLocated(..)
                                                , IE(..)
                                                )
import           Language.Haskell.GHC.ExactPrint.Transform
import           RdrName
import           OccName
import           Language.Haskell.GHC.ExactPrint.Types

import           Avail

import Debug.Trace

-- See https://www.machinesung.com/scribbles/terser-import-declarations.html
-- and https://www.machinesung.com/scribbles/ghc-api.html



mkNamesFromAvailInfos :: [AvailInfo] -> [Name]
mkNamesFromAvailInfos = concatMap availNames -- there are also other choices

mkIEVarFromNameT :: Monad m => Name -> TransformT m (Located (IE GhcPs))
mkIEVarFromNameT name = do
  loc <- uniqueSrcSpanT
  traceM $ "loc: " ++ show loc
  return $ L
    loc
    (IEVar noExt
           (L loc (IEName (L loc (mkVarUnqual ((occNameFS . occName) name)))))
    )

addExportDeclAnnT :: Monad m => Located (IE GhcPs) -> TransformT m ()
addExportDeclAnnT (L _ (IEVar _ (L _ (IEName x)))) =
  addSimpleAnnT x (DP (1, 2)) [(G AnnVal, DP (0, 0))]

addCommaT :: Monad m => Located (IE GhcPs) -> TransformT m ()
addCommaT x@(L _ (IEVar _ (L _ (IEName _)))) =
  addSimpleAnnT x (DP (0, 0)) [(G AnnComma, DP (0, 0))]
