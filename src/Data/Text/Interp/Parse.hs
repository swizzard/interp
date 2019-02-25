{- |
    Moduel : Data.Text.Interp.Parse

    Parsing types and functions
-}
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

-- | Parse `Data.Text.Interp.Types.IText`s
itP :: Parser (NonEmpty IText)
itP = NE.fromList <$> (some $ try $ toInterpolateP <|> rawTextP)

interpStart = "{{"
interpStop = "}}"

-- | parse raw text (i.e., text without interpolations)
rawTextP :: Parser IText
rawTextP = RawText <$> takeWhile1P (Just "raw text") (\c -> not (c `elem` ['}', '{']))

-- | parse text that needs interpolation
toInterpolateP :: Parser IText
toInterpolateP = between (string interpStart >> space)
                         (space >> string interpStop)
                 interP

-- | parse interpolation instructions (with or without binding)
interP :: Parser IText
interP = try $ bindP <|> keyP

-- | parse interpolation that needs binding
bindP :: Parser IText
bindP = do
  char '('
  bk <- key <* char '#'
  bp <- NE.fromList <$> keys <* string ")"
  path <- (optional (char '.')) *> keys
  let b = Just $ Binding bk bp
  return $ ToInterpolate b path

-- | parse interpolation that doesn't need binding
keyP :: Parser IText
keyP = ToInterpolate Nothing <$> keys

-- | parse a single key
key :: Parser Key
key = Key <$> pack <$> some (alphaNumChar <|> satisfy (`elem` ['_', '-'])) <?>
          "keys can only contain letters, numbers, _, and -"

-- | parse a list of keys
keys :: Parser [Key]
keys = sepBy key (char '.')
