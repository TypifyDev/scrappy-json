module Scrappy.JSON.Primitives
  ( -- * JSON structure parsers (return raw String)
    jsonObject
  , jsonArray
  , jsonString
  , jsonStringBody
  , jsonNumber
  , jsonBool
  , jsonNull
  , jsonValue

    -- * Internal helpers (exported for power users)
  , balancedBraces
  , balancedBrackets
  , jsonStringChar
  ) where

import Text.Parsec (Parsec, char, anyChar, noneOf, try, (<|>), many, many1, string, option, oneOf, digit)

-- | Parse a balanced JSON object { ... }, handling nested braces and strings.
jsonObject :: Parsec String u String
jsonObject = do
  _ <- char '{'
  inner <- balancedBraces 1
  pure $ "{" ++ inner

-- | Parse a balanced JSON array [ ... ], handling nested brackets and strings.
jsonArray :: Parsec String u String
jsonArray = do
  _ <- char '['
  inner <- balancedBrackets 1
  pure $ "[" ++ inner

-- | Parse a JSON string including surrounding quotes.
jsonString :: Parsec String u String
jsonString = do
  _ <- char '"'
  cs <- many jsonStringChar
  _ <- char '"'
  pure $ "\"" ++ concat cs ++ "\""

-- | Parse a JSON string and return only the unescaped content (no quotes).
jsonStringBody :: Parsec String u String
jsonStringBody = do
  _ <- char '"'
  cs <- many jsonStringChar
  _ <- char '"'
  pure $ unescape (concat cs)
  where
    unescape [] = []
    unescape ('\\':'"':rest)  = '"'  : unescape rest
    unescape ('\\':'\\':rest) = '\\' : unescape rest
    unescape ('\\':'/':rest)  = '/'  : unescape rest
    unescape ('\\':'n':rest)  = '\n' : unescape rest
    unescape ('\\':'t':rest)  = '\t' : unescape rest
    unescape ('\\':'r':rest)  = '\r' : unescape rest
    unescape ('\\':'b':rest)  = '\b' : unescape rest
    unescape ('\\':'f':rest)  = '\f' : unescape rest
    unescape (c:rest)         = c    : unescape rest

-- | Parse a JSON number (integer or decimal, with optional exponent).
jsonNumber :: Parsec String u String
jsonNumber = do
  sign <- option "" (string "-")
  int' <- many1 digit
  frac <- option "" $ do
    d <- char '.'
    ds <- many1 digit
    pure (d : ds)
  ex <- option "" $ do
    e <- oneOf "eE"
    s <- option "" (string "+" <|> string "-")
    ds <- many1 digit
    pure (e : s ++ ds)
  pure $ sign ++ int' ++ frac ++ ex

-- | Parse a JSON boolean (true or false).
jsonBool :: Parsec String u String
jsonBool = try (string "true") <|> string "false"

-- | Parse a JSON null.
jsonNull :: Parsec String u String
jsonNull = string "null"

-- | Parse any JSON value.
jsonValue :: Parsec String u String
jsonValue = try jsonObject
        <|> try jsonArray
        <|> try jsonString
        <|> try jsonNumber
        <|> try jsonBool
        <|> jsonNull

-- | Parse a single character or escape sequence inside a JSON string.
jsonStringChar :: Parsec String u String
jsonStringChar =
  try (do _ <- char '\\'; c <- anyChar; pure ['\\', c])
  <|> (pure <$> noneOf "\"\\")

-- | Consume characters maintaining brace balance, respecting JSON strings.
balancedBraces :: Int -> Parsec String u String
balancedBraces 0 = pure ""
balancedBraces n = do
  c <- anyChar
  case c of
    '{' -> (c :) <$> balancedBraces (n + 1)
    '}' -> if n == 1 then pure "}" else (c :) <$> balancedBraces (n - 1)
    '"' -> do
      s <- jsonStringInner
      ((c : s) ++) <$> balancedBraces n
    _   -> (c :) <$> balancedBraces n

-- | Consume characters maintaining bracket balance, respecting JSON strings.
balancedBrackets :: Int -> Parsec String u String
balancedBrackets 0 = pure ""
balancedBrackets n = do
  c <- anyChar
  case c of
    '[' -> (c :) <$> balancedBrackets (n + 1)
    ']' -> if n == 1 then pure "]" else (c :) <$> balancedBrackets (n - 1)
    '"' -> do
      s <- jsonStringInner
      ((c : s) ++) <$> balancedBrackets n
    _   -> (c :) <$> balancedBrackets n

-- | Parse the body of a JSON string (after opening quote), returning raw chars + closing quote.
jsonStringInner :: Parsec String u String
jsonStringInner = do
  cs <- many jsonStringChar
  _ <- char '"'
  pure (concat cs ++ "\"")
