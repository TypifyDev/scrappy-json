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
  ( Parsec, char, string, try, (<|>), sepBy
  , parserZero, option
  )
import qualified Text.Parsec as P

-- | Parse whitespace between JSON tokens.
jsonWhitespace :: Parsec String u ()
jsonWhitespace = P.skipMany (P.oneOf " \t\n\r")

-- | Parse a specific named field from a JSON object.
-- Expects to be positioned inside an object (after '{' or ',').
-- Skips over other key-value pairs until the named field is found.
field :: String -> Parsec String u a -> Parsec String u a
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
optionalField :: String -> Parsec String u a -> Parsec String u (Maybe a)
optionalField name valueParser =
  option Nothing (Just <$> try (field name valueParser))

-- | Parse a JSON string value and return the unescaped Haskell String.
jString :: Parsec String u String
jString = jsonStringBody

-- | Parse a JSON integer value.
jInt :: Parsec String u Int
jInt = do
  s <- jsonNumber
  case reads s of
    [(n, "")] -> pure n
    _         -> parserZero

-- | Parse a JSON integer value as Integer.
jInteger :: Parsec String u Integer
jInteger = do
  s <- jsonNumber
  case reads s of
    [(n, "")] -> pure n
    _         -> parserZero

-- | Parse a JSON number as Double.
jDouble :: Parsec String u Double
jDouble = do
  s <- jsonNumber
  case reads s of
    [(n, "")] -> pure n
    _         -> parserZero

-- | Parse a JSON boolean and return a Haskell Bool.
jBool :: Parsec String u Bool
jBool = (try (string "true") >> pure True)
    <|> (string "false" >> pure False)

-- | Parse a JSON null and return ().
jNull :: Parsec String u ()
jNull = string "null" >> pure ()

-- | Parse a JSON array, applying the element parser to each element.
jArray :: Parsec String u a -> Parsec String u [a]
jArray elemParser = do
  _ <- char '['
  jsonWhitespace
  elems <- (jsonWhitespace >> elemParser) `sepBy` (jsonWhitespace >> char ',')
  jsonWhitespace
  _ <- char ']'
  pure elems
