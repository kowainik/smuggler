module Smuggler.Parser
  ( runParser
  )
where

import           Language.Haskell.GHC.ExactPrint
                                                ( Anns )
import           Language.Haskell.GHC.ExactPrint.Parsers
                                                ( parseModuleFromString )
import           HsExtension                    ( GhcPs )
import           HsSyn                          ( HsModule(..) )
import           SrcLoc                         ( Located )


runParser
  :: FilePath -> String -> IO (Either () (Anns, Located (HsModule GhcPs)))
runParser fileName fileContents = do
  res <- parseModuleFromString fileName fileContents
  pure $ case res of
    Left  (_srcSpan, _str) -> Left ()
    Right x                -> Right x

-- newtype ParseException = ParseException String
--     deriving (Show)
--
-- instance Exception ParseException
