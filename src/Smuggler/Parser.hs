module Smuggler.Parser
       ( runParser
       ) where

import Language.Haskell.GHC.ExactPrint (Anns, parseModule)

import HsExtension (GhcPs)
import HsSyn (HsModule (..))
import SrcLoc (Located)

runParser :: FilePath -> IO (Either () (Anns, Located (HsModule GhcPs)))
runParser fileName = do
    res <- parseModule fileName
    pure $ case res of
        Left (_srcSpan, _str) -> Left ()
        Right x               -> Right x

-- newtype ParseException = ParseException String
--     deriving (Show)
--
-- instance Exception ParseException
