module Smuggler.Name
       ( moduleBodyNames
       ) where

import Data.Generics.Schemes (listify)
import GHC (GenLocated (..), ParsedSource)
import HsSyn (HsModule (..))
import OccName (OccName)

moduleBodyNames :: ParsedSource -> [OccName]
moduleBodyNames (L _ ast) = ordNub $ listify (\(_ :: OccName) -> True) (hsmodDecls ast)
