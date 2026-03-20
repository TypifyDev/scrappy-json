{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog

import GHC.Generics (Generic)
import Data.Char (chr)
import Data.Either (isLeft, isRight)
import Data.List (isInfixOf)
import Scrappy.JSON
import Text.Parsec (char, parse, eof)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "scrappy-json"
  [ testGroup "Primitives" primitivesTests
  , testGroup "Value" valueTests
  , testGroup "Record" recordTests
  , testGroup "Generic" genericTests
  , testGroup "Edge Cases" edgeCaseTests
  , testGroup "Error Reporting" errorReportingTests
  ]

-- ============================================================
-- Primitives Tests
-- ============================================================

primitivesTests :: [TestTree]
primitivesTests =
  [ testProperty "jsonObject parses simple object" prop_jsonObject_simple
  , testProperty "jsonObject handles nested objects" prop_jsonObject_nested
  , testProperty "jsonObject handles strings with braces" prop_jsonObject_string_braces
  , testProperty "jsonArray parses simple array" prop_jsonArray_simple
  , testProperty "jsonArray handles nested arrays" prop_jsonArray_nested
  , testProperty "jsonString parses string with escapes" prop_jsonString_escapes
  , testProperty "jsonStringBody returns unescaped content" prop_jsonStringBody
  , testProperty "jsonNumber parses integers" prop_jsonNumber_int
  , testProperty "jsonNumber parses decimals" prop_jsonNumber_decimal
  , testProperty "jsonNumber parses exponents" prop_jsonNumber_exp
  , testProperty "jsonBool parses true and false" prop_jsonBool
  , testProperty "jsonNull parses null" prop_jsonNull
  , testProperty "jsonValue parses any value type" prop_jsonValue
  ]

prop_jsonObject_simple :: Property
prop_jsonObject_simple = withTests 1 $ property $ do
  let input = "{\"key\": \"value\"}"
  case parse jsonObject "" input of
    Right r -> r === input
    Left e  -> do annotate (show e); failure

prop_jsonObject_nested :: Property
prop_jsonObject_nested = withTests 1 $ property $ do
  let input = "{\"outer\": {\"inner\": 42}}"
  case parse jsonObject "" input of
    Right r -> r === input
    Left e  -> do annotate (show e); failure

prop_jsonObject_string_braces :: Property
prop_jsonObject_string_braces = withTests 1 $ property $ do
  let input = "{\"code\": \"function() { return {} }\"}"
  case parse jsonObject "" input of
    Right r -> r === input
    Left e  -> do annotate (show e); failure

prop_jsonArray_simple :: Property
prop_jsonArray_simple = withTests 1 $ property $ do
  let input = "[1, 2, 3]"
  case parse jsonArray "" input of
    Right r -> r === input
    Left e  -> do annotate (show e); failure

prop_jsonArray_nested :: Property
prop_jsonArray_nested = withTests 1 $ property $ do
  let input = "[[1, 2], [3, 4]]"
  case parse jsonArray "" input of
    Right r -> r === input
    Left e  -> do annotate (show e); failure

prop_jsonString_escapes :: Property
prop_jsonString_escapes = withTests 1 $ property $ do
  let input = "\"hello \\\"world\\\" \\n\""
  case parse jsonString "" input of
    Right r -> r === input
    Left e  -> do annotate (show e); failure

prop_jsonStringBody :: Property
prop_jsonStringBody = withTests 1 $ property $ do
  let input = "\"hello \\\"world\\\"\""
  case parse jsonStringBody "" input of
    Right r -> r === "hello \"world\""
    Left e  -> do annotate (show e); failure

prop_jsonNumber_int :: Property
prop_jsonNumber_int = withTests 1 $ property $ do
  case parse jsonNumber "" "42" of
    Right r -> r === "42"
    Left e  -> do annotate (show e); failure
  case parse jsonNumber "" "-7" of
    Right r -> r === "-7"
    Left e  -> do annotate (show e); failure

prop_jsonNumber_decimal :: Property
prop_jsonNumber_decimal = withTests 1 $ property $ do
  case parse jsonNumber "" "3.14" of
    Right r -> r === "3.14"
    Left e  -> do annotate (show e); failure

prop_jsonNumber_exp :: Property
prop_jsonNumber_exp = withTests 1 $ property $ do
  case parse jsonNumber "" "1.5e10" of
    Right r -> r === "1.5e10"
    Left e  -> do annotate (show e); failure
  case parse jsonNumber "" "2E-3" of
    Right r -> r === "2E-3"
    Left e  -> do annotate (show e); failure

prop_jsonBool :: Property
prop_jsonBool = withTests 1 $ property $ do
  case parse jsonBool "" "true" of
    Right r -> r === "true"
    Left e  -> do annotate (show e); failure
  case parse jsonBool "" "false" of
    Right r -> r === "false"
    Left e  -> do annotate (show e); failure

prop_jsonNull :: Property
prop_jsonNull = withTests 1 $ property $ do
  case parse jsonNull "" "null" of
    Right r -> r === "null"
    Left e  -> do annotate (show e); failure

prop_jsonValue :: Property
prop_jsonValue = withTests 1 $ property $ do
  case parse jsonValue "" "{\"a\": 1}" of
    Right _ -> success
    Left e  -> do annotate (show e); failure
  case parse jsonValue "" "[1, 2]" of
    Right _ -> success
    Left e  -> do annotate (show e); failure
  case parse jsonValue "" "\"hello\"" of
    Right _ -> success
    Left e  -> do annotate (show e); failure
  case parse jsonValue "" "42" of
    Right _ -> success
    Left e  -> do annotate (show e); failure

-- ============================================================
-- Value Tests (JValue intermediate type + FromJValue)
-- ============================================================

valueTests :: [TestTree]
valueTests =
  [ testProperty "parseJValue parses string" prop_jvalue_string
  , testProperty "parseJValue parses number" prop_jvalue_number
  , testProperty "parseJValue parses bool" prop_jvalue_bool
  , testProperty "parseJValue parses null" prop_jvalue_null
  , testProperty "parseJValue parses array" prop_jvalue_array
  , testProperty "parseJValue parses object" prop_jvalue_object
  , testProperty "parseJValue parses nested object" prop_jvalue_nested
  , testProperty ".: extracts fields" prop_field_access
  , testProperty ".:? handles missing fields" prop_optional_field_access
  , testProperty "FromJValue record like Aeson" prop_fromjvalue_record
  , testProperty "decode works end to end" prop_decode
  , testProperty "eitherDecode returns error on bad input" prop_eitherDecode
  , testProperty "withObject like Aeson" prop_withObject
  ]

prop_jvalue_string :: Property
prop_jvalue_string = withTests 1 $ property $ do
  case parse parseJValue "" "\"hello\"" of
    Right v -> v === JString "hello"
    Left e  -> do annotate (show e); failure

prop_jvalue_number :: Property
prop_jvalue_number = withTests 1 $ property $ do
  case parse parseJValue "" "42" of
    Right v -> v === JNumber "42"
    Left e  -> do annotate (show e); failure
  case parse parseJValue "" "3.14" of
    Right v -> v === JNumber "3.14"
    Left e  -> do annotate (show e); failure

prop_jvalue_bool :: Property
prop_jvalue_bool = withTests 1 $ property $ do
  case parse parseJValue "" "true" of
    Right v -> v === JBool True
    Left e  -> do annotate (show e); failure
  case parse parseJValue "" "false" of
    Right v -> v === JBool False
    Left e  -> do annotate (show e); failure

prop_jvalue_null :: Property
prop_jvalue_null = withTests 1 $ property $ do
  case parse parseJValue "" "null" of
    Right v -> v === JNull
    Left e  -> do annotate (show e); failure

prop_jvalue_array :: Property
prop_jvalue_array = withTests 1 $ property $ do
  case parse parseJValue "" "[1, 2, 3]" of
    Right v -> v === JArray [JNumber "1", JNumber "2", JNumber "3"]
    Left e  -> do annotate (show e); failure

prop_jvalue_object :: Property
prop_jvalue_object = withTests 1 $ property $ do
  case parse parseJValue "" "{\"name\": \"Alice\", \"age\": 30}" of
    Right (JObject pairs) -> do
      pairs .: "name" === Just ("Alice" :: String)
      pairs .: "age"  === Just (30 :: Int)
    Right v -> do annotate ("Expected JObject, got: " ++ show v); failure
    Left e  -> do annotate (show e); failure

prop_jvalue_nested :: Property
prop_jvalue_nested = withTests 1 $ property $ do
  let input = "{\"user\": {\"name\": \"Bob\", \"scores\": [10, 20, 30]}}"
  case parse parseJValue "" input of
    Right (JObject outer) -> do
      case lookup "user" outer of
        Just (JObject inner) -> do
          inner .: "name" === Just ("Bob" :: String)
          inner .: "scores" === Just ([10, 20, 30] :: [Int])
        _ -> do annotate "Missing user field"; failure
    Right v -> do annotate ("Expected JObject, got: " ++ show v); failure
    Left e  -> do annotate (show e); failure

prop_field_access :: Property
prop_field_access = withTests 1 $ property $ do
  let pairs = [("title", JString "Matrix"), ("rating", JNumber "9")]
  (pairs .: "title")  === Just ("Matrix" :: String)
  (pairs .: "rating") === Just (9 :: Int)
  (pairs .: "missing" :: Maybe String) === Nothing

prop_optional_field_access :: Property
prop_optional_field_access = withTests 1 $ property $ do
  let pairs = [("name", JString "Alice")]
  (pairs .:? "name")    === Just (Just ("Alice" :: String))
  (pairs .:? "missing") === (Just Nothing :: Maybe (Maybe String))

prop_fromjvalue_record :: Property
prop_fromjvalue_record = withTests 1 $ property $ do
  let input = "{\"title\": \"The Matrix\", \"rating\": 9, \"tags\": [\"sci-fi\", \"action\"]}"
  case parse parseJValue "" input of
    Right (JObject obj) -> do
      let result = (,,)
            <$> obj .: "title"
            <*> obj .: "rating"
            <*> obj .: "tags"
      result === Just ("The Matrix" :: String, 9 :: Int, ["sci-fi", "action"] :: [String])
    Right v -> do annotate ("Expected JObject, got: " ++ show v); failure
    Left e  -> do annotate (show e); failure

prop_decode :: Property
prop_decode = withTests 1 $ property $ do
  let input = "{\"title\": \"The Matrix\", \"rating\": 9, \"tags\": [\"sci-fi\", \"action\"]}"
      result = decode input >>= withObject "Movie" (\obj ->
        (,,) <$> obj .: "title" <*> obj .: "rating" <*> obj .: "tags")
  result === Just ("The Matrix" :: String, 9 :: Int, ["sci-fi", "action"] :: [String])

prop_eitherDecode :: Property
prop_eitherDecode = withTests 1 $ property $ do
  case eitherDecode "not json" :: Either String JValue of
    Left _  -> success
    Right _ -> do annotate "Expected Left on bad input"; failure
  case eitherDecode "42" :: Either String Int of
    Right n -> n === 42
    Left e  -> do annotate e; failure

prop_withObject :: Property
prop_withObject = withTests 1 $ property $ do
  let input = "{\"name\": \"Alice\", \"age\": 30}"
      result = decode input >>= withObject "Person" (\obj ->
        (,) <$> obj .: "name" <*> obj .: "age")
  result === Just ("Alice" :: String, 30 :: Int)
  -- withObject on non-object returns Nothing
  let bad = decode "42" >>= withObject "Nope" (\_ -> Just ())
  bad === Nothing

-- ============================================================
-- Record Tests
-- ============================================================

recordTests :: [TestTree]
recordTests =
  [ testProperty "field extracts string field" prop_field_string
  , testProperty "field extracts int field" prop_field_int
  , testProperty "field extracts bool field" prop_field_bool
  , testProperty "field works with multiple fields" prop_field_multiple
  , testProperty "field skips unrelated fields" prop_field_skip
  , testProperty "jArray parses string array" prop_jArray_strings
  , testProperty "jArray parses int array" prop_jArray_ints
  , testProperty "jArray parses nested objects" prop_jArray_nested
  , testProperty "jDouble parses decimals" prop_jDouble
  , testProperty "optionalField returns Nothing for missing" prop_optionalField
  , testProperty "full record parsing without FromJSON" prop_record_no_aeson
  ]

prop_field_string :: Property
prop_field_string = withTests 1 $ property $ do
  let input = "{\"name\": \"Alice\"}"
      parser = do _ <- char '{'; field "name" jString
  case parse parser "" input of
    Right r -> r === "Alice"
    Left e  -> do annotate (show e); failure

prop_field_int :: Property
prop_field_int = withTests 1 $ property $ do
  let input = "{\"count\": 42}"
      parser = do _ <- char '{'; field "count" jInt
  case parse parser "" input of
    Right r -> r === 42
    Left e  -> do annotate (show e); failure

prop_field_bool :: Property
prop_field_bool = withTests 1 $ property $ do
  let input = "{\"active\": true}"
      parser = do _ <- char '{'; field "active" jBool
  case parse parser "" input of
    Right r -> r === True
    Left e  -> do annotate (show e); failure

prop_field_multiple :: Property
prop_field_multiple = withTests 1 $ property $ do
  let input = "{\"name\": \"Bob\", \"age\": 25, \"active\": true}"
      parser = do
        _ <- char '{'
        n <- field "name"   jString
        a <- field "age"    jInt
        v <- field "active" jBool
        pure (n, a, v)
  case parse parser "" input of
    Right r -> r === ("Bob", 25, True)
    Left e  -> do annotate (show e); failure

prop_field_skip :: Property
prop_field_skip = withTests 1 $ property $ do
  let input = "{\"ignored\": \"stuff\", \"name\": \"Alice\", \"extra\": 99, \"age\": 30}"
      parser = do
        _ <- char '{'
        n <- field "name" jString
        a <- field "age"  jInt
        pure (n, a)
  case parse parser "" input of
    Right r -> r === ("Alice", 30)
    Left e  -> do annotate (show e); failure

prop_jArray_strings :: Property
prop_jArray_strings = withTests 1 $ property $ do
  let input = "[\"a\", \"b\", \"c\"]"
  case parse (jArray jString) "" input of
    Right r -> r === ["a", "b", "c"]
    Left e  -> do annotate (show e); failure

prop_jArray_ints :: Property
prop_jArray_ints = withTests 1 $ property $ do
  let input = "[1, 2, 3]"
  case parse (jArray jInt) "" input of
    Right r -> r === [1, 2, 3]
    Left e  -> do annotate (show e); failure

prop_jArray_nested :: Property
prop_jArray_nested = withTests 1 $ property $ do
  let input = "{\"tags\": [\"haskell\", \"parsec\"]}"
      parser = do _ <- char '{'; field "tags" (jArray jString)
  case parse parser "" input of
    Right r -> r === ["haskell", "parsec"]
    Left e  -> do annotate (show e); failure

prop_jDouble :: Property
prop_jDouble = withTests 1 $ property $ do
  case parse jDouble "" "3.14" of
    Right r -> assert $ abs (r - 3.14) < 0.001
    Left e  -> do annotate (show e); failure

prop_optionalField :: Property
prop_optionalField = withTests 1 $ property $ do
  let input = "{\"name\": \"Alice\"}"
      parser = do
        _ <- char '{'
        n <- field "name" jString
        a <- optionalField "age" jInt
        pure (n, a)
  case parse parser "" input of
    Right (n, a) -> do
      n === "Alice"
      a === Nothing
    Left e -> do annotate (show e); failure

prop_record_no_aeson :: Property
prop_record_no_aeson = withTests 1 $ property $ do
  let input = "{\"title\": \"The Matrix\", \"rating\": 9, \"tags\": [\"sci-fi\", \"action\"], \"summary\": \"A mind-bending film\"}"
      parser = do
        _ <- char '{'
        t <- field "title"   jString
        r <- field "rating"  jInt
        g <- field "tags"    (jArray jString)
        s <- field "summary" jString
        pure (t, r, g, s)
  case parse parser "" input of
    Right (t, r, g, s) -> do
      t === "The Matrix"
      r === 9
      g === ["sci-fi", "action"]
      s === "A mind-bending film"
    Left e -> do
      annotate (show e)
      failure

-- ============================================================
-- Generic Tests
-- ============================================================

-- Record type
data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show, Eq, Generic)

instance FromJValue Person

-- Newtype
newtype Wrapper = Wrapper String deriving (Show, Eq, Generic)

instance FromJValue Wrapper

-- Sum type with nullary constructors
data Color = Red | Green | Blue deriving (Show, Eq, Generic)

instance FromJValue Color

-- Sum type with record constructors
data Shape
  = Circle    { radius :: Double }
  | Rectangle { width :: Double, height :: Double }
  deriving (Show, Eq, Generic)

instance FromJValue Shape

genericTests :: [TestTree]
genericTests =
  [ testProperty "generic record" prop_generic_record
  , testProperty "generic newtype" prop_generic_newtype
  , testProperty "generic nullary sum" prop_generic_nullary_sum
  , testProperty "generic record sum (Circle)" prop_generic_sum_circle
  , testProperty "generic record sum (Rectangle)" prop_generic_sum_rectangle
  , testProperty "generic decode end-to-end" prop_generic_decode
  , testProperty "generic returns Nothing on wrong shape" prop_generic_nothing
  ]

prop_generic_record :: Property
prop_generic_record = withTests 1 $ property $ do
  let input = "{\"name\": \"Alice\", \"age\": 30}"
  decode input === Just (Person "Alice" 30)

prop_generic_newtype :: Property
prop_generic_newtype = withTests 1 $ property $ do
  decode "\"hello\"" === Just (Wrapper "hello")

prop_generic_nullary_sum :: Property
prop_generic_nullary_sum = withTests 1 $ property $ do
  decode "\"Red\""   === Just Red
  decode "\"Green\"" === Just Green
  decode "\"Blue\""  === Just Blue
  (decode "\"Purple\"" :: Maybe Color) === Nothing

prop_generic_sum_circle :: Property
prop_generic_sum_circle = withTests 1 $ property $ do
  let input = "{\"radius\": 5.0}"
  decode input === Just (Circle 5.0)

prop_generic_sum_rectangle :: Property
prop_generic_sum_rectangle = withTests 1 $ property $ do
  let input = "{\"width\": 3.0, \"height\": 4.0}"
  decode input === Just (Rectangle 3.0 4.0)

prop_generic_decode :: Property
prop_generic_decode = withTests 1 $ property $ do
  let input = "{\"name\": \"Bob\", \"age\": 25}"
  case eitherDecode input of
    Right (p :: Person) -> do
      name p === "Bob"
      age p === 25
    Left e -> do annotate e; failure

prop_generic_nothing :: Property
prop_generic_nothing = withTests 1 $ property $ do
  (decode "42" :: Maybe Person) === Nothing
  (decode "{\"wrong\": 1}" :: Maybe Person) === Nothing

-- ============================================================
-- Edge Case Tests
-- ============================================================

edgeCaseTests :: [TestTree]
edgeCaseTests =
  [ testGroup "Empty structures"
    [ testProperty "empty object" prop_empty_object
    , testProperty "empty array" prop_empty_array
    , testProperty "empty string" prop_empty_string
    ]
  , testGroup "Unicode escapes"
    [ testProperty "\\uXXXX basic" prop_unicode_basic
    , testProperty "\\uXXXX in jsonString preserves raw" prop_unicode_raw
    , testProperty "\\uXXXX surrogate pair" prop_unicode_surrogate
    , testProperty "\\uXXXX mixed with regular escapes" prop_unicode_mixed
    ]
  , testGroup "Deep nesting"
    [ testProperty "deeply nested objects" prop_deep_objects
    , testProperty "deeply nested arrays" prop_deep_arrays
    ]
  , testGroup "Numbers"
    [ testProperty "large integer" prop_large_int
    , testProperty "very small decimal" prop_tiny_decimal
    , testProperty "extreme exponent" prop_extreme_exp
    , testProperty "negative zero" prop_negative_zero
    , testProperty "leading zeros rejected" prop_leading_zeros
    ]
  , testGroup "Whitespace"
    [ testProperty "tabs and newlines in object" prop_ws_tabs_newlines
    , testProperty "no whitespace compact JSON" prop_no_whitespace
    ]
  , testGroup "Duplicate keys"
    [ testProperty "(.:) returns first match" prop_duplicate_keys
    ]
  , testGroup "Malformed JSON rejection"
    [ testProperty "unclosed brace" prop_malformed_unclosed_brace
    , testProperty "unclosed bracket" prop_malformed_unclosed_bracket
    , testProperty "unclosed string" prop_malformed_unclosed_string
    , testProperty "jBool rejects capitalized" prop_bool_case_sensitive
    , testProperty "bare word rejected as value" prop_bare_word
    , testProperty "invalid escape sequence rejected" prop_invalid_escape
    , testProperty "control chars in string rejected" prop_control_chars
    ]
  , testGroup "Field parser edge cases"
    [ testProperty "field at end of object" prop_field_at_end
    , testProperty "optionalField present" prop_optionalField_present
    , testProperty "FromJValue type mismatch" prop_fromjvalue_type_mismatch
    ]
  , testGroup "Generic edge cases"
    [ testProperty "generic with extra fields" prop_generic_extra_fields
    , testProperty "generic sum ambiguity" prop_generic_sum_ambiguity
    , testProperty "generic Maybe field" prop_generic_maybe_field
    ]
  ]

-- Empty structures

prop_empty_object :: Property
prop_empty_object = withTests 1 $ property $ do
  case parse parseJValue "" "{}" of
    Right v -> v === JObject []
    Left e  -> do annotate (show e); failure

prop_empty_array :: Property
prop_empty_array = withTests 1 $ property $ do
  case parse parseJValue "" "[]" of
    Right v -> v === JArray []
    Left e  -> do annotate (show e); failure

prop_empty_string :: Property
prop_empty_string = withTests 1 $ property $ do
  case parse parseJValue "" "\"\"" of
    Right v -> v === JString ""
    Left e  -> do annotate (show e); failure

-- Unicode escapes

prop_unicode_basic :: Property
prop_unicode_basic = withTests 1 $ property $ do
  -- \u0041 is 'A'
  case parse jsonStringBody "" "\"\\u0041\"" of
    Right r -> r === "A"
    Left e  -> do annotate (show e); failure
  -- \u00E9 is 'é'
  case parse jsonStringBody "" "\"\\u00e9\"" of
    Right r -> r === "\xe9"
    Left e  -> do annotate (show e); failure

prop_unicode_raw :: Property
prop_unicode_raw = withTests 1 $ property $ do
  -- jsonString preserves raw escape sequences
  case parse jsonString "" "\"\\u0041\"" of
    Right r -> r === "\"\\u0041\""
    Left e  -> do annotate (show e); failure

prop_unicode_surrogate :: Property
prop_unicode_surrogate = withTests 1 $ property $ do
  -- U+1F600 (😀) = \uD83D\uDE00 as surrogate pair
  case parse jsonStringBody "" "\"\\uD83D\\uDE00\"" of
    Right r -> r === [chr 0x1F600]
    Left e  -> do annotate (show e); failure

prop_unicode_mixed :: Property
prop_unicode_mixed = withTests 1 $ property $ do
  -- Mix of \uXXXX and regular escapes
  case parse jsonStringBody "" "\"\\u0048ello\\nworld\"" of
    Right r -> r === "Hello\nworld"
    Left e  -> do annotate (show e); failure

-- Deep nesting

prop_deep_objects :: Property
prop_deep_objects = withTests 1 $ property $ do
  -- 10 levels of nested objects
  let nested = foldr (\i rest -> "{\"l" ++ show (i :: Int) ++ "\": " ++ rest ++ "}") "42" [1..10]
  case parse jsonObject "" nested of
    Right r -> r === nested
    Left e  -> do annotate (show e); failure

prop_deep_arrays :: Property
prop_deep_arrays = withTests 1 $ property $ do
  -- 10 levels of nested arrays
  let nested = replicate 10 '[' ++ "1" ++ replicate 10 ']'
  case parse jsonArray "" nested of
    Right r -> r === nested
    Left e  -> do annotate (show e); failure

-- Numbers

prop_large_int :: Property
prop_large_int = withTests 1 $ property $ do
  let big = "99999999999999999999"
  case parse jsonNumber "" big of
    Right r -> r === big
    Left e  -> do annotate (show e); failure

prop_tiny_decimal :: Property
prop_tiny_decimal = withTests 1 $ property $ do
  case parse jsonNumber "" "0.000001" of
    Right r -> r === "0.000001"
    Left e  -> do annotate (show e); failure

prop_extreme_exp :: Property
prop_extreme_exp = withTests 1 $ property $ do
  case parse jsonNumber "" "1e308" of
    Right r -> r === "1e308"
    Left e  -> do annotate (show e); failure
  case parse jsonNumber "" "5e-324" of
    Right r -> r === "5e-324"
    Left e  -> do annotate (show e); failure

prop_negative_zero :: Property
prop_negative_zero = withTests 1 $ property $ do
  case parse jsonNumber "" "-0" of
    Right r -> r === "-0"
    Left e  -> do annotate (show e); failure

prop_leading_zeros :: Property
prop_leading_zeros = withTests 1 $ property $ do
  -- RFC 8259: int = zero / ( digit1-9 *DIGIT ) — "07" is invalid.
  -- jsonNumber parses "0" and stops; "07" is not consumed as one token.
  case parse (jsonNumber >> eof) "" "07" of
    Right _ -> do annotate "Should not parse 07 as a single number"; failure
    Left _  -> success
  -- "0" alone is valid
  case parse jsonNumber "" "0" of
    Right r -> r === "0"
    Left e  -> do annotate (show e); failure
  -- "0.5" is valid
  case parse jsonNumber "" "0.5" of
    Right r -> r === "0.5"
    Left e  -> do annotate (show e); failure

-- Whitespace

prop_ws_tabs_newlines :: Property
prop_ws_tabs_newlines = withTests 1 $ property $ do
  let input = "{\n\t\"name\"\t:\r\n\t\"Alice\"\n}"
  case parse parseJValue "" input of
    Right (JObject pairs) -> pairs .: "name" === Just ("Alice" :: String)
    Right v -> do annotate ("Expected JObject, got: " ++ show v); failure
    Left e  -> do annotate (show e); failure

prop_no_whitespace :: Property
prop_no_whitespace = withTests 1 $ property $ do
  let input = "{\"a\":1,\"b\":[2,3],\"c\":{\"d\":true}}"
  case parse parseJValue "" input of
    Right (JObject pairs) -> do
      pairs .: "a" === Just (1 :: Int)
      pairs .: "b" === Just ([2, 3] :: [Int])
    Right v -> do annotate ("Expected JObject, got: " ++ show v); failure
    Left e  -> do annotate (show e); failure

-- Duplicate keys

prop_duplicate_keys :: Property
prop_duplicate_keys = withTests 1 $ property $ do
  let input = "{\"a\": 1, \"a\": 2}"
  case parse parseJValue "" input of
    Right (JObject pairs) -> do
      -- (.:) uses lookup, which returns the first match
      pairs .: "a" === Just (1 :: Int)
    Right v -> do annotate ("Expected JObject, got: " ++ show v); failure
    Left e  -> do annotate (show e); failure

-- Malformed JSON rejection

prop_malformed_unclosed_brace :: Property
prop_malformed_unclosed_brace = withTests 1 $ property $ do
  assert $ isLeft (parse (jsonObject >> eof) "" "{\"a\": 1")

prop_malformed_unclosed_bracket :: Property
prop_malformed_unclosed_bracket = withTests 1 $ property $ do
  assert $ isLeft (parse (jsonArray >> eof) "" "[1, 2")

prop_malformed_unclosed_string :: Property
prop_malformed_unclosed_string = withTests 1 $ property $ do
  assert $ isLeft (parse (jsonString >> eof) "" "\"hello")

prop_bool_case_sensitive :: Property
prop_bool_case_sensitive = withTests 1 $ property $ do
  assert $ isLeft (parse jsonBool "" "True")
  assert $ isLeft (parse jsonBool "" "FALSE")

prop_bare_word :: Property
prop_bare_word = withTests 1 $ property $ do
  assert $ isLeft (parse (parseJValue >> eof) "" "undefined")

prop_invalid_escape :: Property
prop_invalid_escape = withTests 1 $ property $ do
  -- \a is not a valid JSON escape
  assert $ isLeft (parse jsonString "" "\"hello\\aworld\"")
  -- \x is not a valid JSON escape
  assert $ isLeft (parse jsonString "" "\"\\x41\"")
  -- Valid escapes still work
  case parse jsonStringBody "" "\"\\n\\t\\r\\\\\\\"\"" of
    Right r -> r === "\n\t\r\\\""
    Left e  -> do annotate (show e); failure

prop_control_chars :: Property
prop_control_chars = withTests 1 $ property $ do
  -- Tab (0x09) must be escaped, not literal
  assert $ isLeft (parse jsonString "" ("\"hello" ++ ['\x09'] ++ "world\""))
  -- Newline (0x0A) must be escaped, not literal
  assert $ isLeft (parse jsonString "" ("\"hello" ++ ['\x0A'] ++ "world\""))
  -- Null byte (0x00) must be escaped, not literal
  assert $ isLeft (parse jsonString "" ("\"hello" ++ ['\x00'] ++ "world\""))

-- Field parser edge cases

prop_field_at_end :: Property
prop_field_at_end = withTests 1 $ property $ do
  -- Field is the last one in the object, preceded by others
  let input = "{\"x\": 1, \"y\": 2, \"target\": \"found\"}"
      parser = do _ <- char '{'; field "target" jString
  case parse parser "" input of
    Right r -> r === "found"
    Left e  -> do annotate (show e); failure

prop_optionalField_present :: Property
prop_optionalField_present = withTests 1 $ property $ do
  let input = "{\"name\": \"Alice\", \"age\": 30}"
      parser = do
        _ <- char '{'
        n <- field "name" jString
        a <- optionalField "age" jInt
        pure (n, a)
  case parse parser "" input of
    Right (n, a) -> do
      n === "Alice"
      a === Just 30
    Left e -> do annotate (show e); failure

prop_fromjvalue_type_mismatch :: Property
prop_fromjvalue_type_mismatch = withTests 1 $ property $ do
  -- String where Int expected
  (fromJValue (JString "hello") :: Maybe Int) === Nothing
  -- Number where Bool expected
  (fromJValue (JNumber "42") :: Maybe Bool) === Nothing
  -- Object where String expected
  (fromJValue (JObject []) :: Maybe String) === Nothing
  -- Array where Int expected
  (fromJValue (JArray []) :: Maybe Int) === Nothing
  -- Null where String expected
  (fromJValue JNull :: Maybe String) === Nothing

-- Generic edge cases

data PersonWithEmail = PersonWithEmail
  { pwe_name  :: String
  , pwe_email :: String
  } deriving (Show, Eq, Generic)

instance FromJValue PersonWithEmail where
  fromJValue = withObject "PersonWithEmail" $ \obj ->
    PersonWithEmail <$> obj .: "pwe_name" <*> obj .: "pwe_email"

prop_generic_extra_fields :: Property
prop_generic_extra_fields = withTests 1 $ property $ do
  -- Extra fields should be silently ignored by generic deriving
  let input = "{\"name\": \"Alice\", \"age\": 30, \"city\": \"NYC\"}"
  decode input === Just (Person "Alice" 30)

prop_generic_sum_ambiguity :: Property
prop_generic_sum_ambiguity = withTests 1 $ property $ do
  -- Circle and Rectangle both take JObject, but different fields
  -- Circle has "radius", Rectangle has "width" + "height"
  let circleInput = "{\"radius\": 3.0}"
      rectInput   = "{\"width\": 2.0, \"height\": 5.0}"
  (decode circleInput :: Maybe Shape) === Just (Circle 3.0)
  (decode rectInput :: Maybe Shape)   === Just (Rectangle 2.0 5.0)

data MaybeRecord = MaybeRecord
  { mr_name :: String
  , mr_note :: Maybe String
  } deriving (Show, Eq, Generic)

instance FromJValue MaybeRecord where
  fromJValue = withObject "MaybeRecord" $ \obj ->
    MaybeRecord <$> obj .: "mr_name" <*> obj .:? "mr_note"

prop_generic_maybe_field :: Property
prop_generic_maybe_field = withTests 1 $ property $ do
  -- With the optional field present
  let with' = "{\"mr_name\": \"Alice\", \"mr_note\": \"hi\"}"
  decode with' === Just (MaybeRecord "Alice" (Just "hi"))
  -- Without the optional field
  let without = "{\"mr_name\": \"Bob\"}"
  decode without === Just (MaybeRecord "Bob" Nothing)

-- ============================================================
-- Error Reporting Tests
-- ============================================================

errorReportingTests :: [TestTree]
errorReportingTests =
  [ testProperty "jvalueType returns correct names" prop_jvalueType
  , testProperty "(.:!) missing key" prop_err_missing_key
  , testProperty "(.:!) type mismatch" prop_err_type_mismatch
  , testProperty "(.:!) success" prop_err_field_success
  , testProperty "(.:?!) missing key returns Right Nothing" prop_err_optional_missing
  , testProperty "(.:?!) type mismatch returns Left" prop_err_optional_mismatch
  , testProperty "(.:?!) success" prop_err_optional_success
  , testProperty "eitherDecode parse error" prop_err_parse_error
  , testProperty "eitherDecode type error" prop_err_decode_type
  , testProperty "eitherDecode success" prop_err_decode_success
  , testProperty "withObjectE wrong type" prop_err_withObject
  , testProperty "withArrayE wrong type" prop_err_withArray
  , testProperty "withStringE wrong type" prop_err_withString
  , testProperty "withNumberE wrong type" prop_err_withNumber
  , testProperty "withBoolE wrong type" prop_err_withBool
  , testProperty "error messages are informative" prop_err_messages_informative
  ]

prop_jvalueType :: Property
prop_jvalueType = withTests 1 $ property $ do
  jvalueType (JObject []) === "Object"
  jvalueType (JArray [])  === "Array"
  jvalueType (JString "") === "String"
  jvalueType (JNumber "0") === "Number"
  jvalueType (JBool True) === "Bool"
  jvalueType JNull        === "Null"

prop_err_missing_key :: Property
prop_err_missing_key = withTests 1 $ property $ do
  let obj = [("name", JString "Alice")]
  case (obj .:! "age" :: Either String Int) of
    Left e  -> assert $ "not found" `isInfixOf` e
    Right _ -> do annotate "Should have failed"; failure

prop_err_type_mismatch :: Property
prop_err_type_mismatch = withTests 1 $ property $ do
  let obj = [("name", JString "Alice")]
  case (obj .:! "name" :: Either String Int) of
    Left e  -> assert $ "String" `isInfixOf` e
    Right _ -> do annotate "Should have failed"; failure

prop_err_field_success :: Property
prop_err_field_success = withTests 1 $ property $ do
  let obj = [("name", JString "Alice"), ("age", JNumber "30")]
  (obj .:! "name") === Right ("Alice" :: String)
  (obj .:! "age")  === Right (30 :: Int)

prop_err_optional_missing :: Property
prop_err_optional_missing = withTests 1 $ property $ do
  let obj = [("name", JString "Alice")]
  (obj .:?! "age") === Right (Nothing :: Maybe Int)

prop_err_optional_mismatch :: Property
prop_err_optional_mismatch = withTests 1 $ property $ do
  let obj = [("name", JString "Alice")]
  case (obj .:?! "name" :: Either String (Maybe Int)) of
    Left e  -> assert $ "String" `isInfixOf` e
    Right _ -> do annotate "Should have failed"; failure

prop_err_optional_success :: Property
prop_err_optional_success = withTests 1 $ property $ do
  let obj = [("name", JString "Alice")]
  (obj .:?! "name") === Right (Just ("Alice" :: String))

prop_err_parse_error :: Property
prop_err_parse_error = withTests 1 $ property $ do
  case (eitherDecode "not json" :: Either String JValue) of
    Left e  -> assert $ "parse error" `isInfixOf` e
    Right _ -> do annotate "Should have failed"; failure

prop_err_decode_type :: Property
prop_err_decode_type = withTests 1 $ property $ do
  case (eitherDecode "42" :: Either String String) of
    Left e  -> assert $ "Number" `isInfixOf` e
    Right _ -> do annotate "Should have failed"; failure

prop_err_decode_success :: Property
prop_err_decode_success = withTests 1 $ property $ do
  assert $ isRight (eitherDecode "42" :: Either String Int)
  assert $ isRight (eitherDecode "\"hello\"" :: Either String String)

prop_err_withObject :: Property
prop_err_withObject = withTests 1 $ property $ do
  case withObjectE "test" (\_ -> Right ()) (JString "nope") of
    Left e  -> do
      assert $ "test" `isInfixOf` e
      assert $ "String" `isInfixOf` e
    Right _ -> do annotate "Should have failed"; failure

prop_err_withArray :: Property
prop_err_withArray = withTests 1 $ property $ do
  case withArrayE "arr" (\_ -> Right ()) (JNumber "42") of
    Left e  -> do
      assert $ "arr" `isInfixOf` e
      assert $ "Number" `isInfixOf` e
    Right _ -> do annotate "Should have failed"; failure

prop_err_withString :: Property
prop_err_withString = withTests 1 $ property $ do
  case withStringE "str" (\_ -> Right ()) (JBool True) of
    Left e  -> do
      assert $ "str" `isInfixOf` e
      assert $ "Bool" `isInfixOf` e
    Right _ -> do annotate "Should have failed"; failure

prop_err_withNumber :: Property
prop_err_withNumber = withTests 1 $ property $ do
  case withNumberE "num" (\_ -> Right ()) JNull of
    Left e  -> do
      assert $ "num" `isInfixOf` e
      assert $ "Null" `isInfixOf` e
    Right _ -> do annotate "Should have failed"; failure

prop_err_withBool :: Property
prop_err_withBool = withTests 1 $ property $ do
  case withBoolE "flag" (\_ -> Right ()) (JArray []) of
    Left e  -> do
      assert $ "flag" `isInfixOf` e
      assert $ "Array" `isInfixOf` e
    Right _ -> do annotate "Should have failed"; failure

prop_err_messages_informative :: Property
prop_err_messages_informative = withTests 1 $ property $ do
  -- (.:!) includes the key name in error messages
  let obj = [("x", JString "hello")]
  case (obj .:! "x" :: Either String Int) of
    Left e  -> do
      assert $ "\"x\"" `isInfixOf` e
      assert $ "String" `isInfixOf` e
    Right _ -> do annotate "Should have failed"; failure
  case (obj .:! "missing" :: Either String Int) of
    Left e  -> assert $ "\"missing\"" `isInfixOf` e
    Right _ -> do annotate "Should have failed"; failure
