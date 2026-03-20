# scrappy-json

A Parsec-based JSON library for Haskell that extracts and parses JSON from noisy text. Designed for use with LLM output where JSON is embedded in natural language responses.

## Why not aeson?

aeson is the standard JSON library for Haskell, but it requires:
- The entire input to be valid JSON
- ByteString input (not String)
- Template Haskell or manual instances for custom types

scrappy-json is designed for a different use case:
- **Noisy input**: Extract JSON embedded in prose, markdown, or LLM output
- **String-native**: Works directly with `String` and Parsec — no ByteString conversion
- **Lightweight**: Only depends on `base` and `parsec`
- **RFC 8259 compliant**: The JSON parsing itself is strict per spec
- **Generic deriving**: `FromJValue` instances derived via `GHC.Generics` — no TH required

## Modules

| Module | Purpose |
|--------|---------|
| `Scrappy.JSON` | Re-exports everything below |
| `Scrappy.JSON.Primitives` | Low-level parsers returning raw `String` (for extraction from noisy text) |
| `Scrappy.JSON.Value` | `JValue` intermediate type, `FromJValue` typeclass, `decode`/`eitherDecode` |
| `Scrappy.JSON.Record` | Applicative field-by-field parsing (alternative to `FromJValue`) |

## Quick Start

### Decoding with Generic deriving

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Scrappy.JSON

data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show, Generic)

instance FromJValue Person

main :: IO ()
main = do
  let json = "{\"name\": \"Alice\", \"age\": 30}"
  print (decode json :: Maybe Person)
  -- Just (Person {name = "Alice", age = 30})
```

### Manual FromJValue with error reporting

```haskell
data Movie = Movie String Int [String]

instance FromJValue Movie where
  fromJValue = withObject "Movie" $ \obj ->
    Movie <$> obj .: "title"
          <*> obj .: "year"
          <*> obj .: "genres"

-- With error messages using the Either-based API:
parseMovie :: String -> Either String Movie
parseMovie input = do
  v <- case eitherDecode input of
    Right jv -> Right jv
    Left e   -> Left e
  withObjectE "Movie" (\obj ->
    Movie <$> obj .:! "title"
          <*> obj .:! "year"
          <*> obj .:! "genres") v
```

### Applicative field-by-field parsing (Record module)

When you want fine-grained control over parsing order or need to parse fields from a stream:

```haskell
import Scrappy.JSON
import Text.Parsec (parse, char)

parseUser :: String -> Either String (String, Int)
parseUser input =
  case parse parser "" input of
    Left e  -> Left (show e)
    Right r -> Right r
  where
    parser = do
      _ <- char '{'
      n <- field "name" jString
      a <- field "age"  jInt
      pure (n, a)
```

The `field` parser skips unrelated fields, so field order in the JSON doesn't matter.

### Extracting JSON from noisy text

The Primitives module returns raw JSON strings, useful for extracting JSON from LLM output:

```haskell
import Scrappy.JSON
import Text.Parsec (parse, anyChar, many, try)

-- Extract the first JSON object from noisy text
findObject :: String -> Maybe String
findObject input =
  case parse (many (try (anyChar >> pure ())) >> jsonObject) "" input of
    Right obj -> Just obj
    Left _    -> Nothing
```

For more robust extraction from noisy text, use this with [scrappy-core](https://github.com/TypifyDev/scrappy-core) which provides `scrapeFirst` and `scrapeAll`.

## API Reference

### JValue

Intermediate JSON representation:

```haskell
data JValue
  = JObject [(String, JValue)]
  | JArray  [JValue]
  | JString String
  | JNumber String    -- raw number string, not parsed to Double
  | JBool   Bool
  | JNull
```

Numbers are stored as raw strings to avoid precision loss.

### FromJValue typeclass

```haskell
class FromJValue a where
  fromJValue :: JValue -> Maybe a
```

Built-in instances: `String`, `Int`, `Integer`, `Double`, `Bool`, `()` (for null), `[a]`, `JValue`.

Types with a `Generic` instance get `fromJValue` for free via `genericFromJValue`.

### Field access operators

| Operator | Type | Description |
|----------|------|-------------|
| `(.:)` | `[(String, JValue)] -> String -> Maybe a` | Required field (Maybe) |
| `(.:?)` | `[(String, JValue)] -> String -> Maybe (Maybe a)` | Optional field (Maybe) |
| `(.:!)` | `[(String, JValue)] -> String -> Either String a` | Required field (with error message) |
| `(.:?!)` | `[(String, JValue)] -> String -> Either String (Maybe a)` | Optional field (with error message) |

### Decoding functions

| Function | Type | Description |
|----------|------|-------------|
| `decode` | `String -> Maybe a` | Parse and convert, no error info |
| `eitherDecode` | `String -> Either String a` | Parse and convert, with error messages |

### with* combinators

Maybe-based: `withObject`, `withArray`, `withString`, `withNumber`, `withBool`

Either-based (with error messages): `withObjectE`, `withArrayE`, `withStringE`, `withNumberE`, `withBoolE`

### Record module parsers

| Parser | Type | Description |
|--------|------|-------------|
| `field` | `String -> Parsec String u a -> Parsec String u a` | Extract named field |
| `optionalField` | `String -> Parsec String u a -> Parsec String u (Maybe a)` | Extract optional field |
| `jString` | `Parsec String u String` | Parse JSON string |
| `jInt` | `Parsec String u Int` | Parse JSON integer |
| `jInteger` | `Parsec String u Integer` | Parse JSON integer (unbounded) |
| `jDouble` | `Parsec String u Double` | Parse JSON decimal |
| `jBool` | `Parsec String u Bool` | Parse JSON boolean |
| `jNull` | `Parsec String u ()` | Parse JSON null |
| `jArray` | `Parsec String u a -> Parsec String u [a]` | Parse JSON array |

## JSON Spec Compliance (RFC 8259)

scrappy-json is fully compliant with RFC 8259:

- **Numbers**: No leading zeros (`07` is rejected; `0` and `0.5` are valid)
- **Strings**: Only valid escape sequences (`\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`, `\t`, `\uXXXX`)
- **Unicode**: Full `\uXXXX` support including surrogate pairs for characters outside the BMP
- **Control characters**: Unescaped control characters (< 0x20) are rejected in strings
- **Whitespace**: Only space, tab, newline, and carriage return are valid whitespace
- **Booleans**: Case-sensitive (`true`/`false` only)

## Generic Deriving Details

### Records

Field names in the Haskell record must match JSON keys exactly:

```haskell
data Config = Config { host :: String, port :: Int }
  deriving (Generic)
instance FromJValue Config
-- Parses: {"host": "localhost", "port": 8080}
```

Extra fields in the JSON are silently ignored.

### Newtypes

Newtypes unwrap directly:

```haskell
newtype UserId = UserId String deriving (Generic)
instance FromJValue UserId
-- Parses: "abc123" -> UserId "abc123"
```

### Sum types with nullary constructors

Matched by constructor name as a JSON string:

```haskell
data Color = Red | Green | Blue deriving (Generic)
instance FromJValue Color
-- Parses: "Red" -> Red, "Blue" -> Blue
```

### Sum types with record constructors

Each constructor is tried in order:

```haskell
data Shape
  = Circle    { radius :: Double }
  | Rectangle { width :: Double, height :: Double }
  deriving (Generic)
instance FromJValue Shape
-- {"radius": 5.0}          -> Circle 5.0
-- {"width": 3, "height": 4} -> Rectangle 3.0 4.0
```
