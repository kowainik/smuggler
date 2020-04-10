{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances    #-}

module Smuggler.Import
       ( Import (..)
       , getLocationMap
       ) where

import BasicTypes (StringLiteral (..))
import HsImpExp (IE (..), IEWildcard (..), IEWrappedName (..), ImportDecl (..), LIE, LIEWrappedName,
                 LImportDecl)
import HsSyn (HsModule (..))
import Module (ModuleName, moduleName, moduleNameString)
import Name (nameOccName)
import OccName (occNameString)
import RdrName (RdrName (..))
import SrcLoc (GenLocated (L), Located, SrcSpan)

import Smuggler.Loc (showL, showLoc)

import qualified Data.HashMap.Strict as HashMap
import qualified Text.Show as Show (show)

data Import name = Import
    { impName      :: Located ModuleName -- ^ Module name.
    , impPkgQual   :: Maybe StringLiteral  -- ^ Package qualifier.
    , impQualified :: Bool          -- ^ True => qualified
    , impImplicit  :: Bool          -- ^ True => implicit import (of Prelude)
    , impAs        :: Maybe (Located ModuleName)  -- ^ as Module
    , impHiding    :: Maybe (Bool, Located [LIE name]) -- ^ (True => hiding, names)
    , impId        :: Int -- ^ For hashing
    } deriving (Eq, Generic)

instance (Show name) => Show (Import name) where
    show i = let L _ md = impName i in "Module: " ++ moduleNameString md ++
        case impHiding i of
            Just (b, L _ lies) -> " " ++ show b ++ " " ++ showLIEs lies
            Nothing            -> ""

showLIEs :: (Show name) => [LIE name] -> String
showLIEs []   = ""
showLIEs lies = "[ " ++ intercalate ", " (map showLIE lies) ++"]"

showLIE :: forall name . (Show name) => LIE name -> String
showLIE (L _ ie) = showIE ie
  where
    showIE :: IE name -> String
    showIE (IEVar lieWrappedName)       = showL " IEVar " showIEWrappedName lieWrappedName
    showIE (IEThingAbs lieWrappedName)  = showL " IEThingAbs " showIEWrappedName lieWrappedName
    showIE (IEThingAll lieWrappedName)  = showL " IEThingAll " showIEWrappedName lieWrappedName
    showIE (IEThingWith lieWrappedName wildCard lieWNs _) = showL " IEThingWith " showIEWrappedName lieWrappedName
        ++ showIEWildCard wildCard
        ++ "[" ++ intercalate ", " (map showLIEWrappedName lieWNs) ++ "]"
    showIE _ = "Not going to be"

    showLIEWrappedName :: LIEWrappedName name -> String
    showLIEWrappedName (L l ieWrappedName) = showLoc l ++ showIEWrappedName ieWrappedName

    showIEWrappedName :: IEWrappedName name -> String
    showIEWrappedName (IEName located)    = showL " IEName " show located
    showIEWrappedName (IEPattern located) = showL " IEPattern " show located
    showIEWrappedName (IEType located)    = showL " IEType " show located


    showIEWildCard :: IEWildcard -> String
    showIEWildCard NoIEWildcard   = ""
    showIEWildCard (IEWildcard n) = "(..)" ++ show n

instance Show RdrName where
    show (Unqual occName)       = occNameString occName
    show (Qual modName occName) = moduleNameString modName ++ " " ++ occNameString occName
    show (Orig md occName)      = moduleNameString (moduleName md) ++ " " ++ occNameString occName
    show (Exact nm)             = occNameString $ nameOccName nm

instance Hashable (Import name) where
    hashWithSalt salt = hashWithSalt salt . impId

getLocationMap :: (Eq name) => HsModule name -> HashMap (Import name) SrcSpan
getLocationMap HsModule{..} =  HashMap.fromList $ fromDecls hsmodImports
  where
    fromDecls :: [LImportDecl name] -> [(Import name, SrcSpan)]
    fromDecls lidecls =  zipWith (curry toImportPairs) lidecls [0 .. ]

    toImportPairs :: (LImportDecl name, Int) -> (Import name, SrcSpan)
    toImportPairs (L l impDecl, i) = (toMyImport impDecl i, l)

    toMyImport :: ImportDecl name -> Int -> Import name
    toMyImport ImportDecl{..} i = Import
        { impName      = ideclName
        , impPkgQual   = ideclPkgQual
        , impQualified = ideclQualified
        , impImplicit  = ideclImplicit
        , impAs        = ideclAs
        , impHiding    = ideclHiding
        , impId        = i
        }
