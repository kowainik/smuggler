{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Smuggler.Debug
  ( debugAST
  )
where

import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           Data.Text.Lazy                 ( toStrict )
import           Data.Typeable                  ( Typeable
                                                , typeRep
                                                )
import           Fmt                            ( fmt
                                                , padLeftF
                                                )
import           Text.Pretty.Simple             ( pShow )
import qualified Data.Text                     as T
                                                ( lines
                                                , pack
                                                , unlines
                                                )
import qualified Data.Text.IO                  as T
                                                ( putStrLn )


-- | Helper function to debug different parts of AST processing.
{-# WARNING debugAST "'debugAST' remains in code" #-}
debugAST :: forall a . (Show a, Typeable a) => a -> IO ()
debugAST ast = do
    let header = "==================== || " <> typeName @a <> " || ====================\n"
    T.putStrLn $ (header <>)
               $ T.unlines
               $ zipWith (\i line -> lineNumber i <> "| " <> line) [1..]
               $ T.lines
               $ toStrict
               $ pShow ast
  where
    lineNumber :: Int -> Text
    lineNumber = fmt . padLeftF 4 ' '

typeName :: forall a . Typeable a => Text
typeName = T.pack $ show $ typeRep $ Proxy @a
