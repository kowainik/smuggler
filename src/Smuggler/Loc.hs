module Smuggler.Loc
       ( showLoc
       , showL
       , unL
       ) where

import SrcLoc (GenLocated (L), Located, SrcSpan (RealSrcSpan), srcSpanStartCol, srcSpanStartLine,
               unLoc)

-- | Returns location in the way of @line:col@.
showLoc :: SrcSpan -> String
showLoc (RealSrcSpan r) = show (srcSpanStartLine r) ++ ":" ++ show (srcSpanStartCol r)
showLoc loc             = show loc

-- | Shows 'Located' in brackets.
showL :: String -> (a -> String) -> Located a -> String
showL name showA (L l nm) = "(" ++ showLoc l ++ name ++ showA nm ++ ")"

-- | Shorter synonim for 'unLoc'.
unL :: GenLocated l e -> e
unL = unLoc
