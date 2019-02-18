{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Text (unpack)
import Options.Applicative
import System.IO

import Data.Text.Interp


main :: IO ()
main = execParser opts >>= run >>= \case
    (Left e) -> hPutStrLn stderr $ unpack e
    (Right v) -> hPutStrLn stdout $ unpack v
