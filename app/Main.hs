{-# LANGUAGE LambdaCase #-}
module Main where

import Options.Applicative
import System.IO

import Data.Text.Interp


main :: IO ()
main = execParser opts >>= run >>= \case
    (Left e) -> hPrint stderr e
    (Right v) -> hPrint stdout v
