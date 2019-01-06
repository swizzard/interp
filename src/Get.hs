{-# LANGUAGE OverloadedStrings #-}
module Get where

import Control.Monad (foldM)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import Data.Random (MonadRandom, StdRandom(..), randomElement, runRVar)
import Data.Text (Text, pack)

import Types (Key(..), Subst)


sample :: MonadRandom m => [a] -> m a
sample xs = runRVar (randomElement xs) StdRandom

getL :: Key -> Subst a -> Maybe [a]
getL k s = case M.lookup k s of
                      Nothing -> Nothing
                      (Just []) -> Nothing
                      (Just xs) -> Just xs

tshow :: Show a => a -> Text
tshow = pack . show

mshow :: Maybe Text -> Text
mshow = maybe " " id

getT :: (MonadRandom m, Show a) => Key -> Subst a -> m Text
getT k s = maybe (return " ") (\x -> tshow <$> sample x) (getL k s)

get :: (MonadRandom m) => Key -> Subst a -> MaybeT m a
get k s = MaybeT $ traverse sample (getL k s)
