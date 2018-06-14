module Smuggler
       ( parseFile
       ) where

import Language.Haskell.GHC.ExactPrint.Parsers (withDynFlags)

import DynFlags (DynFlags)
import FastString (mkFastString)
import HsSyn (HsModule (..))
import Lexer (ParseResult (..), mkPState, unP)
import Outputable (showPpr)
import Parser (parseModule)
import RdrName (RdrName)
import SrcLoc (Located, mkRealSrcLoc)
import StringBuffer (stringToStringBuffer)

parseFile :: IO ()
parseFile = do
    let path = "src/Smuggler.hs"
    fileContent <- readFile path
    dynFlags <- withDynFlags id
    case runParser path dynFlags (toString fileContent) of
        POk _ ast -> putStrLn $ showPpr dynFlags ast
        _         -> putTextLn "Oops :("

runParser :: FilePath -> DynFlags -> String -> ParseResult (Located (HsModule RdrName))
runParser fileName flags str = unP parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString fileName) 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location
