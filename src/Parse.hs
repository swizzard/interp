{-# LANGUAGE DuplicateRecordFields #-}

module Parse where

import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char

import Types (Key(..))

data VarName = Name Text | Anonymous

data Slot = Slot { key :: Key
                 , var :: VarName
                 }
