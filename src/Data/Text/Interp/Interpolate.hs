{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Interp.Interpolate where

import Control.Monad (foldM)
import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text, append)

import Data.Text.Interp.Get
import Data.Text.Interp.Types


data IState = IState {
              acc :: Text
            , bm :: BindingMap
            }

newIState :: IState
newIState = IState "" M.empty

interp :: Subst -> NonEmpty IText -> I Text
interp s its = acc <$> foldM (interp' s) newIState its

interp' :: Subst -> IState -> IText -> I IState
interp' _ (IState a m) (RawText t) = return $ IState (a `append` t) m
interp' s (IState a m) (ToInterpolate Nothing p) = do
  res <- get s p m
  case res of
    (SubstM _) -> throwError "too few keys"
    (SubstL _) -> throwError "too few keys"
    (SubstV x) -> return $ IState (a `append` x) m
interp' s (IState a m) (ToInterpolate (Just (Binding n bp)) p) = do
  toBind <- get s (NE.toList bp) m
  res <- get toBind p m
  case res of
    (SubstM _) -> throwError "too few keys"
    (SubstL _) -> throwError "too few keys"
    (SubstV x) -> return $ IState (a `append` x) $ M.insert n toBind m
