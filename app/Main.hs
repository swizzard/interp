{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.Except
import Data.Text (unpack, Text)
import Options.Applicative
import System.IO

import Data.Text.Interp (doInterp, opts, Input(..))


run :: Input -> IO (Either Text Text)
run (Input sf tf) = runExceptT $ doInterp sf tf


main :: IO ()
main = execParser opts >>= run >>= \case
    (Left e) -> hPutStrLn stderr $ unpack e
    (Right v) -> hPutStrLn stdout $ unpack v
