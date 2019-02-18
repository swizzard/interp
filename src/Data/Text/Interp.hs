module Data.Text.Interp
  ( opts
  , run
  ) where

import Control.Monad.Except
import Control.Monad.Trans (lift)
import Data.Aeson
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import Text.Megaparsec

import Data.Text.Interp.Interpolate (interp)
import Data.Text.Interp.Parse (itP)
import Data.Text.Interp.Types

data Input = Input {
             substF :: String
           , interpF :: String
           }

input ::Parser Input
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

opts :: ParserInfo Input
opts = info (input <**> helper)
          (fullDesc
          <> progDesc "Randomly interpolate values into a template"
          <> header "interp")


run' :: String -> String -> I Text
run' sf tf = do
  i <- parse itP "" <$> T.strip <$> (lift $ T.readFile tf)
  case i of
    (Left e) -> throwError . T.pack $ errorBundlePretty e
    (Right i') -> do
      s <- lift $ eitherDecodeFileStrict sf
      case s of
        (Left e) -> throwError $ T.pack e
        (Right s') -> interp s' i'

run :: Input -> IO (Either Text Text)
run (Input sf tf) = runExceptT $ run' sf tf
