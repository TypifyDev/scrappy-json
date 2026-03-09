{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog

import GHC.Generics (Generic)
import Scrappy.JSON
import Text.Parsec (char, parse)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "scrappy-json"
  [ testGroup "Primitives" primitivesTests
  , testGroup "Value" valueTests
  , testGroup "Record" recordTests
  , testGroup "Generic" genericTests
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
