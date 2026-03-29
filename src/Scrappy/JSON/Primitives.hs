{-# LANGUAGE FlexibleContexts #-}

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

import Data.Char (chr, isHexDigit, digitToInt)
import Text.Parsec (ParsecT, Stream, char, anyChar, try, (<|>), many, many1, string, option, oneOf, digit, satisfy, count)

-- | Parse a balanced JSON object { ... }, handling nested braces and strings.
jsonObject :: (Stream s m Char) => ParsecT s u m String
jsonObject = do
  _ <- char '{'
  inner <- balancedBraces 1
  pure $ "{" ++ inner

-- | Parse a balanced JSON array [ ... ], handling nested brackets and strings.
jsonArray :: (Stream s m Char) => ParsecT s u m String
jsonArray = do
  _ <- char '['
  inner <- balancedBrackets 1
  pure $ "[" ++ inner

-- | Parse a JSON string including surrounding quotes.
jsonString :: (Stream s m Char) => ParsecT s u m String
jsonString = do
  _ <- char '"'
  cs <- many jsonStringChar
  _ <- char '"'
  pure $ "\"" ++ concat cs ++ "\""

-- | Parse a JSON string and return only the unescaped content (no quotes).
jsonStringBody :: (Stream s m Char) => ParsecT s u m String
jsonStringBody = do
  _ <- char '"'
  cs <- many jsonStringChar
  _ <- char '"'
  pure $ unescape (concat cs)
  where
    unescape [] = []
    unescape ('\\':'u':a:b:c:d:rest)
      | all isHexDigit [a,b,c,d] =
          let cp = hexToInt [a,b,c,d]
          in case rest of
               -- Surrogate pair: \uD800-\uDBFF followed by \uDC00-\uDFFF
               ('\\':'u':a2:b2:c2:d2:rest2)
                 | all isHexDigit [a2,b2,c2,d2]
                 , let hi = cp
                 , let lo = hexToInt [a2,b2,c2,d2]
                 , hi >= 0xD800 && hi <= 0xDBFF
                 , lo >= 0xDC00 && lo <= 0xDFFF
                 -> let full = 0x10000 + (hi - 0xD800) * 0x400 + (lo - 0xDC00)
                    in chr full : unescape rest2
               _ -> chr cp : unescape rest
    unescape ('\\':'"':rest)  = '"'  : unescape rest
    unescape ('\\':'\\':rest) = '\\' : unescape rest
    unescape ('\\':'/':rest)  = '/'  : unescape rest
    unescape ('\\':'n':rest)  = '\n' : unescape rest
    unescape ('\\':'t':rest)  = '\t' : unescape rest
    unescape ('\\':'r':rest)  = '\r' : unescape rest
    unescape ('\\':'b':rest)  = '\b' : unescape rest
    unescape ('\\':'f':rest)  = '\f' : unescape rest
    unescape (c:rest)         = c    : unescape rest

    hexToInt :: String -> Int
    hexToInt = foldl (\acc h -> acc * 16 + digitToInt h) 0

-- | Parse a JSON number per RFC 8259.
-- int = zero / ( digit1-9 *DIGIT ) — no leading zeros.
jsonNumber :: (Stream s m Char) => ParsecT s u m String
jsonNumber = do
  sign <- option "" (string "-")
  int' <- jsonInt
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
  where
    -- RFC 8259: int = zero / ( digit1-9 *DIGIT )
    jsonInt = do
      d <- digit
      if d == '0'
        then pure "0"
        else (d :) <$> many digit

-- | Parse a JSON boolean (true or false).
jsonBool :: (Stream s m Char) => ParsecT s u m String
jsonBool = try (string "true") <|> string "false"

-- | Parse a JSON null.
jsonNull :: (Stream s m Char) => ParsecT s u m String
jsonNull = string "null"

-- | Parse any JSON value.
jsonValue :: (Stream s m Char) => ParsecT s u m String
jsonValue = try jsonObject
        <|> try jsonArray
        <|> try jsonString
        <|> try jsonNumber
        <|> try jsonBool
        <|> jsonNull

-- | Parse a single character or escape sequence inside a JSON string per RFC 8259.
-- Only valid escape sequences are accepted: \" \\ \/ \b \f \n \r \t \uXXXX.
-- Unescaped characters must be >= 0x20 (no control characters).
jsonStringChar :: (Stream s m Char) => ParsecT s u m String
jsonStringChar =
  try (do _ <- char '\\'
          c <- anyChar
          case c of
            'u' -> do
              hex4 <- count 4 (satisfy isHexDigit)
              pure $ "\\u" ++ hex4
            _ | c `elem` ("\"\\/bfnrt" :: String) -> pure ['\\', c]
              | otherwise -> fail $ "invalid JSON escape: \\" ++ [c])
  <|> (pure <$> satisfy (\c -> c /= '"' && c /= '\\' && c >= '\x20'))

-- | Consume characters maintaining brace balance, respecting JSON strings.
balancedBraces :: (Stream s m Char) => Int -> ParsecT s u m String
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
balancedBrackets :: (Stream s m Char) => Int -> ParsecT s u m String
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
jsonStringInner :: (Stream s m Char) => ParsecT s u m String
jsonStringInner = do
  cs <- many jsonStringChar
  _ <- char '"'
  pure (concat cs ++ "\"")
