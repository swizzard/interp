{-# LANGUAGE OverloadedStrings #-}
module Get where

import Control.Monad.Except
import Data.Maybe (maybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Random (MonadRandom, StdRandom(..), randomElement, runRVar)
import Data.Text (Text, append)

import Types

data BindingState a = NotYetBound Key -- ^ alias the result should be bound to
                    | Bound (Subst a)
                    deriving (Eq, Show)

type BindingMap a = M.Map Key (BindingState a)

get :: MonadRandom m => Subst a ->
                        [Key] ->
                        BindingMap a ->
                        ExceptT Text m (Subst a, BindingMap a)
get v@(SubstV _) [] m = return (v, m)
get (SubstV _) xs _ = throwError "too many keys"
get (SubstM _) [] _ = throwError "too few keys"
get (SubstM m) (k:ks) bm = case M.lookup k bm of
                             (Just (Bound s')) -> get s' ks bm
                             (Just (NotYetBound alias)) -> do
                               res <- g k m
                               get res ks $ M.insert alias (Bound res) bm
                             Nothing -> do
                               res <- g k m
                               get res ks bm

g :: MonadRandom m => Key -> (M.Map Key (Subst a)) -> ExceptT Text m (Subst a)
g k m = case M.lookup k m of
  Nothing -> throwError $ "bad key: " `append` (unKey k)
  (Just v@(SubstV _)) -> return v
  (Just (SubstL xs)) -> lift $ runRVar (randomElement xs) StdRandom
  (Just m@(SubstM _)) -> return m
