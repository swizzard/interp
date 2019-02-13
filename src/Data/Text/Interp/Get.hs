{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Interp.Get
  ( get
  ) where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import Data.Random (MonadRandom, randomElement)
import Data.RVar (sampleRVar)
import Data.Text (Text, append)

import Data.Text.Interp.Types


get :: Subst -> [Key] -> BindingMap -> I Subst
get v@(SubstV _) [] m = return v
get (SubstV _) xs _ = throwError "too many keys"
get m@(SubstM _) [] _ = return $ m
get (SubstM m) (k:ks) bm = case M.lookup k bm of
                             Nothing -> do
                               res <- g k m
                               get res ks bm
                             (Just s') -> get s' ks bm

g :: Key -> (M.Map Key Subst) -> I Subst
g k m = case M.lookup k m of
  Nothing -> throwError $ "bad key: " `append` (unKey k)
  (Just v@(SubstV _)) -> return v
  (Just (SubstL xs)) -> lift $ sampleRVar (randomElement xs)
  (Just m@(SubstM _)) -> return m
