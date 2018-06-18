module Smuggler.Anns
       ( removeAnnAtLoc
       ) where

import Language.Haskell.GHC.ExactPrint (AnnKey (..), Anns)
import SrcLoc (SrcSpan (RealSrcSpan), srcSpanStartCol, srcSpanStartLine)

import qualified Data.Map.Strict as Map

removeAnnAtLoc :: Int -> Int -> Anns -> Anns
removeAnnAtLoc line col = Map.filterWithKey (\k _ -> matchKey k)
  where
    matchKey :: AnnKey -> Bool
    matchKey (AnnKey (RealSrcSpan rss) _) =
        not (srcSpanStartLine rss == line && srcSpanStartCol rss == col)
    matchKey _ = True
