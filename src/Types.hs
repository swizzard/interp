{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types (Key(..), Subst(..), substm) where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.String (IsString, fromString)
import Data.Text (Text, pack)

newtype Key = Key { unKey :: Text } deriving (Eq, Monoid, Ord, Semigroup, Show)
instance IsString Key where
  fromString = Key . pack

data Subst a = SubstL [Subst a]
             | SubstM (Map Key (Subst a))
             | SubstV a deriving (Eq, Show)

substm :: [(Key, Subst a)] -> Subst a
substm = SubstM . M.fromList


newtype Var = Var { unVar :: Text } deriving (Eq, Ord, Show)
instance IsString Var where
  fromString = Var . pack
