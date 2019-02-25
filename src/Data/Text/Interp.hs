{- |
    Module : Data.Text.Interp

    Main library module
-}
module Data.Text.Interp
  ( opts
  , doInterp
  , Input(..)
  ) where

import Control.Monad.Except
import Control.Monad.Trans (lift)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (FilePath)
import Text.Megaparsec

import Data.Text.Interp.CLI (Input(..), opts)
import Data.Text.Interp.Interpolate (interp)
import Data.Text.Interp.Parse (itP)
import Data.Text.Interp.Types (I)


-- | Generate interpolated text within a `Data.Text.Interp.Types.I` monad
doInterp :: FilePath -- ^ path to substitutions file
         -> FilePath -- ^ path to interpolations file
         -> I Text   -- ^ resulting interpolated text
doInterp sf tf = do
  i <- parse itP "" <$> T.strip <$> (lift $ T.readFile tf)
  case i of
    (Left e) -> throwError . T.pack $ errorBundlePretty e
    (Right i') -> do
      s <- lift $ eitherDecodeFileStrict sf
      case s of
        (Left e) -> throwError $ T.pack e
        (Right s') -> interp s' i'
