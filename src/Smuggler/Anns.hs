module Smuggler.Anns
       ( removeAnnAtLoc
       ) where

import Language.Haskell.GHC.ExactPrint (AnnKey (..), Annotation, Anns)
import SrcLoc (SrcSpan (RealSrcSpan), srcSpanStartCol, srcSpanStartLine)

import qualified Data.Map.Strict as Map

removeAnnAtLoc :: Int -> Int -> Anns -> Anns
removeAnnAtLoc line col = Map.filterWithKey matchKey
  where
    matchKey :: AnnKey -> Annotation -> Bool
    matchKey (AnnKey (RealSrcSpan rss) _) _ =
        not (srcSpanStartLine rss == line && srcSpanStartCol rss == col)
    matchKey _ _ = True
