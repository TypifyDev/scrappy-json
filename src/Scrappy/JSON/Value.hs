{-# LANGUAGE FlexibleInstances #-}

module Scrappy.JSON.Value
  ( -- * JSON value type
    JValue(..)

    -- * Parsing
  , parseJValue

    -- * Conversion
  , FromJValue(..)
  , (.:)
  , (.:?)
  , decode
  , eitherDecode
  , withObject
  , withArray
  , withString
  , withNumber
  , withBool
  ) where

import Scrappy.JSON.Primitives (jsonStringBody, jsonNumber, jsonStringChar)

import Text.Parsec
  ( Parsec, char, string, try, (<|>), many, many1
  , option, oneOf, digit, sepBy, parse
  )
import qualified Text.Parsec as P

-- | Intermediate JSON representation, like Aeson's Value.
data JValue
  = JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber String
  | JBool Bool
  | JNull
  deriving (Show, Eq)

-- | Parse whitespace between JSON tokens.
ws :: Parsec String u ()
ws = P.skipMany (P.oneOf " \t\n\r")

-- | Parse any JSON value into a JValue.
parseJValue :: Parsec String u JValue
parseJValue = try pObject
          <|> try pArray
          <|> try pString
          <|> try pNumber
          <|> try pBool
          <|> pNull

pObject :: Parsec String u JValue
pObject = do
  _ <- char '{'
  ws
  pairs <- pPair `sepBy` (ws >> char ',' >> ws)
  ws
  _ <- char '}'
  pure $ JObject pairs

pPair :: Parsec String u (String, JValue)
pPair = do
  key <- jsonStringBody
  ws
  _ <- char ':'
  ws
  val <- parseJValue
  pure (key, val)

pArray :: Parsec String u JValue
pArray = do
  _ <- char '['
  ws
  vals <- parseJValue `sepBy` (ws >> char ',' >> ws)
  ws
  _ <- char ']'
  pure $ JArray vals

pString :: Parsec String u JValue
pString = JString <$> jsonStringBody

pNumber :: Parsec String u JValue
pNumber = JNumber <$> numStr
  where
    numStr = do
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

pBool :: Parsec String u JValue
pBool = (try (string "true") >> pure (JBool True))
    <|> (string "false" >> pure (JBool False))

pNull :: Parsec String u JValue
pNull = string "null" >> pure JNull

-- | Typeclass for converting JValue to Haskell types.
class FromJValue a where
  fromJValue :: JValue -> Maybe a

instance FromJValue JValue where
  fromJValue = Just

instance {-# OVERLAPPING #-} FromJValue String where
  fromJValue (JString s) = Just s
  fromJValue _ = Nothing

instance FromJValue Int where
  fromJValue (JNumber s) = case reads s of
    [(n, "")] -> Just n
    _ -> Nothing
  fromJValue _ = Nothing

instance FromJValue Integer where
  fromJValue (JNumber s) = case reads s of
    [(n, "")] -> Just n
    _ -> Nothing
  fromJValue _ = Nothing

instance FromJValue Double where
  fromJValue (JNumber s) = case reads s of
    [(n, "")] -> Just n
    _ -> Nothing
  fromJValue _ = Nothing

instance FromJValue Bool where
  fromJValue (JBool b) = Just b
  fromJValue _ = Nothing

instance FromJValue () where
  fromJValue JNull = Just ()
  fromJValue _ = Nothing

instance FromJValue a => FromJValue [a] where
  fromJValue (JArray xs) = traverse fromJValue xs
  fromJValue _ = Nothing

-- | Look up a required field in a JSON object.
(.:) :: FromJValue a => [(String, JValue)] -> String -> Maybe a
obj .: key = lookup key obj >>= fromJValue

-- | Look up an optional field in a JSON object.
(.:?) :: FromJValue a => [(String, JValue)] -> String -> Maybe (Maybe a)
obj .:? key = case lookup key obj of
  Nothing -> Just Nothing
  Just v  -> Just <$> fromJValue v

-- | Decode a JSON string into a Haskell value.
decode :: FromJValue a => String -> Maybe a
decode s = case parse parseJValue "" s of
  Right v -> fromJValue v
  Left _  -> Nothing

-- | Decode a JSON string, returning an error message on failure.
eitherDecode :: FromJValue a => String -> Either String a
eitherDecode s = case parse parseJValue "" s of
  Left e  -> Left (show e)
  Right v -> case fromJValue v of
    Just a  -> Right a
    Nothing -> Left "FromJValue conversion failed"

-- | Apply a function to a JObject's key-value pairs, or fail.
withObject :: String -> ([(String, JValue)] -> Maybe a) -> JValue -> Maybe a
withObject _ f (JObject obj) = f obj
withObject _ _ _ = Nothing

-- | Apply a function to a JArray's elements, or fail.
withArray :: String -> ([JValue] -> Maybe a) -> JValue -> Maybe a
withArray _ f (JArray xs) = f xs
withArray _ _ _ = Nothing

-- | Apply a function to a JString's content, or fail.
withString :: String -> (String -> Maybe a) -> JValue -> Maybe a
withString _ f (JString s) = f s
withString _ _ _ = Nothing

-- | Apply a function to a JNumber's raw string, or fail.
withNumber :: String -> (String -> Maybe a) -> JValue -> Maybe a
withNumber _ f (JNumber s) = f s
withNumber _ _ _ = Nothing

-- | Apply a function to a JBool's value, or fail.
withBool :: String -> (Bool -> Maybe a) -> JValue -> Maybe a
withBool _ f (JBool b) = f b
withBool _ _ _ = Nothing
