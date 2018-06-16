{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Smuggler.Debug
       ( debugAST
       ) where

import Data.Typeable (typeRep)
import Fmt (fmt, padLeftF)
import Text.Pretty.Simple (pShow)

-- | Helper function to debug different parts of AST processing.
{-# WARNING debugAST "'debugAST' remains in code" #-}
debugAST :: forall a . (Show a, Typeable a) => a -> IO ()
debugAST ast = do
    let header = "==================== || " <> typeName @a <> " || ====================\n"
    putTextLn $ (header <>)
              $ unlines
              $ zipWith (\i line -> lineNumber i <> "| " <> line) [1..]
              $ lines
              $ toStrict
              $ pShow ast
  where
    lineNumber :: Int -> Text
    lineNumber = fmt . padLeftF 4 ' '

typeName :: forall a . Typeable a => Text
typeName = show $ typeRep $ Proxy @a
