{- |
    Module : Data.Text.Interp.CLI

    Helper functions and types for CLI use
-}
module Data.Text.Interp.CLI
  ( Input(..)
  , opts
  ) where

import Data.Semigroup ((<>))
import Options.Applicative
import System.IO (FilePath)


-- | Record to hold CLI options
data Input = Input {
             substF :: FilePath -- ^ path to the substitutions file
           , interpF :: FilePath -- ^ path to the interpolations file
           }

-- | CLI parser for `Input`
input :: Parser Input
input = Input <$> (
        (strOption $ long "substitutions"
         <> help "JSON file containing map of substitutions")
        <|>
        (strArgument $ metavar "SUBST"
          <> help "JSON file containing map of substitutions")
        ) <*> (
        (strOption $ long "interpolations"
           <> help "file containing text to interpolate")
        <|>
         (strArgument $ metavar "INTERP"
           <> help "file containing text to interpolate"))

-- | ParserInfo for `Input`
opts :: ParserInfo Input
opts = info (input <**> helper)
          (fullDesc
          <> progDesc "Randomly interpolate values into a template"
          <> header "interp")
