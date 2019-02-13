{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Interp.Parse where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Text.Interp.Types

type Parser = Parsec Void Text

itP :: Parser (NonEmpty IText)
itP = NE.fromList <$> (some $ try $ toInterpolateP <|> rawTextP)

interpStart = "{{"
interpStop = "}}"

rawTextP :: Parser IText
rawTextP = RawText <$> takeWhile1P (Just "raw text") (\c -> not (c `elem` ['}', '{']))

toInterpolateP :: Parser IText
toInterpolateP = between (string interpStart >> space)
                         (space >> string interpStop)
                 interP

interP :: Parser IText
interP = try $ bindP <|> keyP

bindP :: Parser IText
bindP = do
  char '('
  bk <- key <* char '#'
  bp <- NE.fromList <$> keys <* string ")"
  path <- (optional (char '.')) *> keys
  let b = Just $ Binding bk bp
  return $ ToInterpolate b path

keyP :: Parser IText
keyP = ToInterpolate Nothing <$> keys

key :: Parser Key
key = Key <$> pack <$> some (alphaNumChar <|> satisfy (`elem` ['_', '-'])) <?>
          "keys can only contain letters, numbers, _, and -"

keys :: Parser [Key]
keys = sepBy key (char '.')
