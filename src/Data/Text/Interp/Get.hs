{- |
    Module: Data.Text.Interp.Get

    Functions to extract values from Subst mappings
-}
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


-- | Randomly retrieve a value out of a `Data.Text.Interp.Types.Subst` mapping
get :: Subst -- ^ source mapping
    -> [Key] -- ^ a list of `Data.Text.Interp.Types.Key`s leading to the value
    -> BindingMap -- ^ the `Data.Text.Interp.Types.BindingMap` to check for bound values in
    -> I Subst -- ^ retrieved value
get v@(SubstV _) [] m = return v
get (SubstV _) xs _ = throwError "too many keys"
get m@(SubstM _) [] _ = return $ m
get (SubstM m) (k:ks) bm = case M.lookup k bm of
                             Nothing -> do
                               res <- g k m
                               get res ks bm
                             (Just s') -> get s' ks bm

-- | Helper function that digs one layer down in a `Data.Text.Interp.Types.Subst` mapping
g :: Key -> (M.Map Key Subst) -> I Subst
g k m = case M.lookup k m of
  Nothing -> throwError $ "bad key: " `append` (unKey k)
  (Just (SubstL xs)) -> lift $ sampleRVar (randomElement xs)
  (Just v) -> return v
