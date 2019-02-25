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


-- | Record holding intermediary interpolation state
data IState = IState {
              acc :: Text -- ^ interpolated text so far
            , bm :: BindingMap -- ^ mapping of bound variables
            }

newIState :: IState
newIState = IState "" M.empty

-- | Build interpolated text out of a `Data.Text.Interp.Types.Subst` mapping and
-- a list of segments to interpolate
interp :: Subst -- ^ mapping to get substitutions out of
       -> NonEmpty IText -- ^ lits of segments to interpolate
       -> I Text -- ^ final interpolation
interp s its = acc <$> foldM (interp' s) newIState its

-- | Interpolate a single segment
interp' :: Subst -- ^ mapping
        -> IState -- ^ intermediary state
        -> IText -- ^ segment to interpolate
        -> I IState -- ^ updated state, after the segment's been interpolated
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
