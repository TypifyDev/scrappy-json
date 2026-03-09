{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrappy.JSON.Value
  ( -- * JSON value type
    JValue(..)

    -- * Parsing
  , parseJValue

    -- * Conversion
  , FromJValue(..)
  , genericFromJValue
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

import Scrappy.JSON.Primitives (jsonStringBody)

import GHC.Generics
import Text.Parsec
  ( Parsec, char, string, try, (<|>), many1
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
-- Types with a 'Generic' instance get a default implementation for free.
class FromJValue a where
  fromJValue :: JValue -> Maybe a
  default fromJValue :: (Generic a, GFromJValue (Rep a)) => JValue -> Maybe a
  fromJValue = genericFromJValue

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

-- ============================================================
-- Generic deriving for FromJValue
-- ============================================================

-- | Decode a JValue using the Generic representation of a type.
genericFromJValue :: (Generic a, GFromJValue (Rep a)) => JValue -> Maybe a
genericFromJValue v = to <$> gFromJValue v

-- | Internal class for generic traversal of a type's Rep.
class GFromJValue f where
  gFromJValue :: JValue -> Maybe (f p)

-- Datatype metadata — unwrap
instance GFromJValue f => GFromJValue (M1 D c f) where
  gFromJValue v = M1 <$> gFromJValue v

-- Constructor metadata — unwrap (non-nullary constructors)
instance {-# OVERLAPPABLE #-} GFromJValue f => GFromJValue (M1 C c f) where
  gFromJValue v = M1 <$> gFromJValue v

-- Selector metadata — look up field by selector name in JObject
instance (Selector s, FromJValue a) => GFromJValue (M1 S s (K1 R a)) where
  gFromJValue (JObject obj) =
    let name = selName (undefined :: M1 S s (K1 R a) p)
    in case name of
      "" -> Nothing -- no selector name, can't do named lookup
      _  -> do
        val <- lookup name obj
        a <- fromJValue val
        pure $ M1 (K1 a)
  -- Newtype / single positional field: unwrap directly
  gFromJValue v =
    let name = selName (undefined :: M1 S s (K1 R a) p)
    in case name of
      "" -> M1 . K1 <$> fromJValue v
      _  -> Nothing

-- Product — pass the full JValue to both sides (both look up by field name)
instance (GFromJValue a, GFromJValue b) => GFromJValue (a :*: b) where
  gFromJValue v = (:*:) <$> gFromJValue v <*> gFromJValue v

-- Sum — try left, then right
instance (GFromJValue a, GFromJValue b) => GFromJValue (a :+: b) where
  gFromJValue v = case gFromJValue v of
    Just l  -> Just (L1 l)
    Nothing -> R1 <$> gFromJValue v

-- Nullary constructor — match JString against constructor name
instance {-# OVERLAPPING #-} Constructor c => GFromJValue (M1 C c U1) where
  gFromJValue (JString s)
    | s == conName (undefined :: M1 C c U1 p) = Just (M1 U1)
  gFromJValue _ = Nothing

-- Unit — always succeeds (inside a constructor that already matched)
instance GFromJValue U1 where
  gFromJValue _ = Just U1
