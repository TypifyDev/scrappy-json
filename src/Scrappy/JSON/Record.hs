{-# LANGUAGE FlexibleContexts #-}

module Scrappy.JSON.Record
  ( -- * Field extraction from JSON objects
    field
  , optionalField

    -- * Typed value parsers
  , jString
  , jInt
  , jInteger
  , jDouble
  , jBool
  , jNull
  , jArray

    -- * Whitespace
  , jsonWhitespace
  ) where

import Scrappy.JSON.Primitives (jsonStringBody, jsonNumber, jsonValue)

import Text.Parsec
  ( ParsecT, Stream, char, string, try, (<|>), sepBy
  , parserZero, option
  )
import qualified Text.Parsec as P

-- | Parse whitespace between JSON tokens.
jsonWhitespace :: (Stream s m Char) => ParsecT s u m ()
jsonWhitespace = P.skipMany (P.oneOf " \t\n\r")

-- | Parse a specific named field from a JSON object.
-- Expects to be positioned inside an object (after '{' or ',').
-- Skips over other key-value pairs until the named field is found.
field :: (Stream s m Char) => String -> ParsecT s u m a -> ParsecT s u m a
field name valueParser = try matchField <|> (skipField >> field name valueParser)
  where
    matchField = do
      jsonWhitespace
      _ <- option ' ' (char ',')
      jsonWhitespace
      _ <- char '"'
      _ <- string name
      _ <- char '"'
      jsonWhitespace
      _ <- char ':'
      jsonWhitespace
      valueParser
    skipField = do
      jsonWhitespace
      _ <- option ' ' (char ',')
      jsonWhitespace
      _ <- char '"'
      _ <- P.many (P.noneOf "\"")
      _ <- char '"'
      jsonWhitespace
      _ <- char ':'
      jsonWhitespace
      _ <- jsonValue
      pure ()

-- | Like 'field' but returns Nothing if the field is not found.
optionalField :: (Stream s m Char) => String -> ParsecT s u m a -> ParsecT s u m (Maybe a)
optionalField name valueParser =
  option Nothing (Just <$> try (field name valueParser))

-- | Parse a JSON string value and return the unescaped Haskell String.
jString :: (Stream s m Char) => ParsecT s u m String
jString = jsonStringBody

-- | Parse a JSON integer value.
jInt :: (Stream s m Char) => ParsecT s u m Int
jInt = do
  s <- jsonNumber
  case reads s of
    [(n, "")] -> pure n
    _         -> parserZero

-- | Parse a JSON integer value as Integer.
jInteger :: (Stream s m Char) => ParsecT s u m Integer
jInteger = do
  s <- jsonNumber
  case reads s of
    [(n, "")] -> pure n
    _         -> parserZero

-- | Parse a JSON number as Double.
jDouble :: (Stream s m Char) => ParsecT s u m Double
jDouble = do
  s <- jsonNumber
  case reads s of
    [(n, "")] -> pure n
    _         -> parserZero

-- | Parse a JSON boolean and return a Haskell Bool.
jBool :: (Stream s m Char) => ParsecT s u m Bool
jBool = (try (string "true") >> pure True)
    <|> (string "false" >> pure False)

-- | Parse a JSON null and return ().
jNull :: (Stream s m Char) => ParsecT s u m ()
jNull = string "null" >> pure ()

-- | Parse a JSON array, applying the element parser to each element.
jArray :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m [a]
jArray elemParser = do
  _ <- char '['
  jsonWhitespace
  elems <- (jsonWhitespace >> elemParser) `sepBy` try (jsonWhitespace >> char ',')
  jsonWhitespace
  _ <- char ']'
  pure elems
