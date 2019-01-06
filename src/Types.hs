{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types (Key(..), Subst(..)) where

import Data.Map.Strict (Map)
import Data.String (IsString, fromString)
import Data.Text (Text, pack)

newtype Key = Key { unKey :: Text } deriving (Eq, Monoid, Ord, Semigroup, Show)
instance IsString Key where
  fromString = Key . pack

type Subst a = Map Key [a]

newtype Var = Var { unVar :: Text } deriving (Eq, Ord, Show)
instance IsString Var where
  fromString = Var . pack

data Label = Label { key :: Key
                   , var :: Maybe Var
                   }
