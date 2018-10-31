module Smuggler.Anns
       ( removeAnnAtLoc
       , removeTrailingCommas
       ) where

import Data.List (groupBy)
import Language.Haskell.GHC.ExactPrint (AnnKey (..), Annotation (..), Anns)
import Language.Haskell.GHC.ExactPrint.Types (AnnConName (..), DeltaPos, KeywordId (..))
import SrcLoc (SrcSpan (RealSrcSpan), srcSpanStartCol, srcSpanStartLine, srcSpanEndLine)

import qualified Data.Map.Strict as Map
import qualified GHC

removeAnnAtLoc :: Int -> Int -> Anns -> Anns
removeAnnAtLoc line col = Map.filterWithKey (\k _ -> matchKey k)
  where
    matchKey :: AnnKey -> Bool
    matchKey (AnnKey (RealSrcSpan rss) _) =
        not (srcSpanStartLine rss == line && srcSpanStartCol rss == col)
    matchKey _ = True

removeTrailingCommas :: Anns -> Anns
removeTrailingCommas
  = Map.fromList . concatMap removeIfImportDecl
  . groupBy withinSrcSpan . Map.toList
  where
    removeIfImportDecl :: [(AnnKey, Annotation)] -> [(AnnKey, Annotation)]
    removeIfImportDecl gAnns
      | any isImportDecl gAnns = removeTrailingComma gAnns
      | otherwise = gAnns

    removeTrailingComma :: [(AnnKey, Annotation)] -> [(AnnKey, Annotation)]
    removeTrailingComma [] = []
    -- The import anns are held in the shape of "(:) ... (IEName, IEVar, Unqual)"
    -- we want to pattern match on the last entry and remove the list separator ','
    -- if it is present
    removeTrailingComma [x, (annKey, ann), z]
      = [x, (annKey, ann { annsDP = filter (not . isTrailingComma) (annsDP ann) }), z]
    removeTrailingComma (x : xs) = x : removeTrailingComma xs

    isImportDecl :: (AnnKey, Annotation) -> Bool
    isImportDecl (AnnKey _ (CN "ImportDecl"), _) = True
    isImportDecl _ = False

    isTrailingComma :: (KeywordId, DeltaPos) -> Bool
    isTrailingComma (G GHC.AnnComma, _) = True
    isTrailingComma _ = False

    withinSrcSpan :: (AnnKey, Annotation) -> (AnnKey, Annotation) -> Bool
    withinSrcSpan (AnnKey (RealSrcSpan x) _, _) (AnnKey (RealSrcSpan y) _, _)
      = srcSpanStartLine x == srcSpanStartLine y
      || srcSpanEndLine x == srcSpanEndLine y
    withinSrcSpan _ _ = True
