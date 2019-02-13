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

data Subst = SubstL [Subst]
           | SubstM (Map Key Subst)
           | SubstV Text
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

substm :: [(Key, Subst)] -> Subst
substm = SubstM . M.fromList

data Binding = Binding {
               name :: Key
             , pathToBind :: NonEmpty Key
             } deriving (Eq, Show)

data IText = RawText Text
           | ToInterpolate {
                binding :: Maybe Binding
             ,  path :: [Key]
             } deriving (Eq, Show)

type I = ExceptT Text IO

type BindingMap = M.Map Key Subst
