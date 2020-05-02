module Smuggler.Anns
  ( removeAnnAtLoc
  , removeTrailingCommas
  , removeLocatedKeywordT
  )
where

import           Data.List                      ( find
                                                , groupBy
                                                )
import qualified Data.Map.Strict               as Map
                                                ( delete
                                                , filterWithKey
                                                , fromList
                                                , insert
                                                , lookup
                                                , toList
                                                )
import qualified GHC                            ( AnnKeywordId(AnnComma) )
import           Language.Haskell.GHC.ExactPrint
                                                ( AnnKey(..)
                                                , Annotation(..)
                                                , Anns
                                                , modifyAnnsT
                                                , TransformT
                                                )
import           Language.Haskell.GHC.ExactPrint.Types
                                                ( AnnConName(..)
                                                , DeltaPos
                                                , KeywordId(..)
                                                , mkAnnKey
                                                )
import           SrcLoc                         ( Located
                                                , SrcSpan(RealSrcSpan)
                                                , srcSpanEndLine
                                                , srcSpanStartCol
                                                , srcSpanStartLine
                                                )

import           Data.Generics                 as SYB

removeAnnAtLoc :: Int -> Int -> Anns -> Anns
removeAnnAtLoc line col = Map.filterWithKey (\k _ -> matchKey k)
 where
  matchKey :: AnnKey -> Bool
  matchKey (AnnKey (RealSrcSpan rss) _) =
    not (srcSpanStartLine rss == line && srcSpanStartCol rss == col)
  matchKey _ = True

removeLocatedKeywordT
  :: (Data a, Monad m) => KeywordId -> Located a -> TransformT m ()
removeLocatedKeywordT kw ast = modifyAnnsT (removeLocatedKeyword kw ast)

removeLocatedKeyword :: (SYB.Data a) => KeywordId -> Located a -> Anns -> Anns
removeLocatedKeyword kw ast anns = case Map.lookup (mkAnnKey ast) anns of
  Nothing -> anns
  Just an -> case find isKeyword (annsDP an) of
    Nothing -> anns
    Just _  -> Map.insert
      (mkAnnKey ast)
      (an { annsDP = filter (not . isKeyword) (annsDP an) })
      anns
   where
    isKeyword (kw', _) | kw' == kw = True
    isKeyword _                    = False

-- Not needed; using the ghc-exactprint library version
removeTrailingCommas :: Anns -> Anns
removeTrailingCommas =
  Map.fromList
    . concatMap removeIfImportDecl
    . groupBy withinSrcSpan
    . Map.toList
 where
  removeIfImportDecl :: [(AnnKey, Annotation)] -> [(AnnKey, Annotation)]
  removeIfImportDecl gAnns | any isImportDecl gAnns = removeTrailingComma gAnns
                           | otherwise              = gAnns

  removeTrailingComma :: [(AnnKey, Annotation)] -> [(AnnKey, Annotation)]
  removeTrailingComma [] = []
  -- The import anns are held in the shape of "(:) ... (IEName, IEVar, Unqual)"
  -- we want to pattern match on the last entry and remove the list separator ','
  -- if it is present
  removeTrailingComma [x, (annKey, ann), z] =
    [ x
    , (annKey, ann { annsDP = filter (not . isTrailingComma) (annsDP ann) })
    , z
    ]
  removeTrailingComma (x : xs) = x : removeTrailingComma xs

  isImportDecl :: (AnnKey, Annotation) -> Bool
  isImportDecl (AnnKey _ (CN "ImportDecl"), _) = True
  isImportDecl _                               = False

  isTrailingComma :: (KeywordId, DeltaPos) -> Bool
  isTrailingComma (G GHC.AnnComma, _) = True
  isTrailingComma _                   = False

  withinSrcSpan :: (AnnKey, Annotation) -> (AnnKey, Annotation) -> Bool
  withinSrcSpan (AnnKey (RealSrcSpan x) _, _) (AnnKey (RealSrcSpan y) _, _) =
    srcSpanStartLine x
      == srcSpanStartLine y
      || srcSpanEndLine x
      == srcSpanEndLine y
  withinSrcSpan _ _ = True
