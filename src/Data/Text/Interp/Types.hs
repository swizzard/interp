{- |
    Module : Data.Text.Interp.Types

    Types shared among other modules
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Text.Interp.Types
  ( Binding(..)
  , BindingMap
  , I
  , IText(..)
  , Key(..)
  , Subst(..)
  , substm
  ) where

import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.String (IsString, fromString)
import Data.Text (Text, pack)
import qualified Data.Vector as V


newtype Key = Key { unKey :: Text } deriving (Eq, IsString, Monoid, Ord, Semigroup, Show)

-- | The main substitution type, either a list, mapping, or single value
data Subst = SubstL [Subst] -- ^ a list of values, one of which will be randomly selected
           | SubstM (Map Key Subst) -- ^ a mapping of `Key`s to values
           | SubstV Text -- ^ a single value
           deriving (Eq, Show)

instance FromJSON Subst where
  parseJSON (String s) = return $ SubstV s
  parseJSON (Number n) = return $ SubstV . pack . show $ n
  parseJSON (Bool b) = return $ SubstV . pack . show $ b
  parseJSON Null = fail "null not allowed"
  parseJSON (Array a) = SubstL <$> (sequence $ V.toList $ parseJSON <$> a)
  parseJSON (Object o) = SubstM <$> HM.foldlWithKey' g (return M.empty) o where
    g :: Parser (Map Key Subst) -> Text -> Value -> Parser (Map Key Subst)
    -- considered liftM2 but it was hard to read imo
    g p k v = do
      acc <- p
      x <- parseJSON v
      return $ M.insert (Key k) x acc

-- | Helper function to build a `Subst` mapping
substm :: [(Key, Subst)] -> Subst
substm = SubstM . M.fromList

-- | A binding of a `Key` to a path of `Key`s
data Binding = Binding {
               name :: Key
             , pathToBind :: NonEmpty Key
             } deriving (Eq, Show)

-- | A segment of text to interpolate (or not)
data IText = RawText Text -- ^ 'Raw' text that doesn't need interpolation
             -- | Text that requires interpolation
           | ToInterpolate {
                binding :: Maybe Binding -- ^ Optional binding
             ,  path :: [Key] -- ^ Path of `Key`s to the value to interpolate
             } deriving (Eq, Show)

type I = ExceptT Text IO

type BindingMap = M.Map Key Subst
