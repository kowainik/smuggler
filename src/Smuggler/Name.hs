module Smuggler.Name
  ( moduleBodyNames
  )
where

import           Data.Generics.Schemes          ( listify )
import           GHC                            ( GenLocated(..)
                                                , ParsedSource
                                                )
import           HsSyn                          ( HsModule(..) )
import           OccName                        ( OccName )
import           Data.Containers.ListUtils      ( nubOrd )

moduleBodyNames :: ParsedSource -> [OccName]
moduleBodyNames (L _ ast) =
  nubOrd $ listify (\(_ :: OccName) -> True) (hsmodDecls ast)
