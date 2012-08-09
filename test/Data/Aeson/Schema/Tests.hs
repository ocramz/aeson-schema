{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Data.Aeson.Schema.Tests
  ( tests
  ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HU

import Data.Foldable (toList)
import Data.Aeson
import Data.Aeson.Types (emptyObject)
import qualified Data.Aeson.Types
import qualified Data.Attoparsec.Number
import qualified Data.Vector
import Data.Aeson.QQ
import Data.Ratio ((%))
import Data.Text (pack)
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust)
import qualified Data.Map as M

import Data.Aeson.Schema
import Data.Aeson.Schema.Choice

data TestFunctor a = TestFunctor Int a

instance Functor TestFunctor where
  fmap f (TestFunctor i a) = TestFunctor i (f a)

tests :: [Test]
tests =
  [ testCase "parse schema.json" $ do
      schemaBS <- L.readFile "examples/schema.json"
      case decode schemaBS :: Maybe Value of
        Nothing -> HU.assertFailure "JSON syntax error"
        Just val -> case fromJSON val :: Result (Schema String) of
          Error e -> HU.assertFailure e
          Success schema -> do
            Just "http://json-schema.org/schema#" HU.@=? schemaId schema
            0 HU.@=? schemaMinItems schema
  , testCase "followReferences" $ do
      let a = TestFunctor 1 "b"
      let b = TestFunctor 2 "a"
      let m = M.fromList [("a", a), ("b", b)]
      let m' = followReferences m
      HU.assertBool "m' has the members 'a' and 'b'" $ "a" `M.member` m' && "b" `M.member` m'
      case M.lookup "a" m' of
        Just (TestFunctor 1 (Fix (TestFunctor 2 (Fix (TestFunctor 1 (Fix (TestFunctor 2 _))))))) -> return ()
        _ -> HU.assertFailure "didn't produce a mutually recursive data structure"
  , testCase "Foldable instance" $ do
      let schemaWithRef ref = empty { schemaDRef = Just ref }
      let schema = empty
            { schemaType = [Choice2of2 $ schemaWithRef "a"]
            , schemaProperties = H.fromList [("aProperty", schemaWithRef "b")]
            , schemaPatternProperties = let Right p = mkPattern "lorem.+" in [(p, schemaWithRef "c")]
            , schemaAdditionalProperties = Choice3of3 $ schemaWithRef "d"
            , schemaItems = Just $ Choice3of3 [schemaWithRef "e", schemaWithRef "f"]
            , schemaAdditionalItems = Choice3of3 $ schemaWithRef "g"
            , schemaDependencies = H.fromList [("aProperty", Choice2of2 $ schemaWithRef "h")]
            , schemaDisallow = [Choice2of2 $ schemaWithRef "i"]
            , schemaExtends = [schemaWithRef "j"]
            , schemaDRef = Just "k"
            }
      map (:[]) ['a'..'k'] HU.@=? toList schema
  , testGroup "validate" validationTests
  ]

validationTests :: [Test]
validationTests =
  [ testCase "type: \"number\"" $ do
      let schema = [aesonQQ| { "type": "number" } |]
      assertValid schema [aesonQQ| 3 |]
      assertValid schema [aesonQQ| 3.5 |]
      assertInvalid schema [aesonQQ| true |]
      assertInvalid schema [aesonQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonQQ| { eins: 1, zwei: 2 } |]
      assertInvalid schema [aesonQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonQQ| null |]
  , testCase "type: \"integer\"" $ do
      let schema = [aesonQQ| { "type": "integer" } |]
      -- unfortunately, we can't use aesonQQ to test integer validation
      -- because aesonQQ makes no distinction between integers and floating-point
      -- numbers
      assertValid schema (Number $ fromInteger 3)
      assertInvalid schema [aesonQQ| 3.5 |]
      assertInvalid schema [aesonQQ| true |]
      assertInvalid schema [aesonQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid schema [aesonQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonQQ| null |]
  , testCase "maximum and minimum" $ do
      let schemaMinimum3 = [aesonQQ| { "type": "number", "minimum": 3 } |]
      assertInvalid schemaMinimum3 [aesonQQ| 2 |]
      assertValid schemaMinimum3 [aesonQQ| 3 |]
      assertValid schemaMinimum3 [aesonQQ| 4 |]
      let schemaExclusiveMinimum3 = [aesonQQ| {
            "type": "number",
            "minimum": 3,
            "exclusiveMinimum": true
          }]
      assertInvalid schemaExclusiveMinimum3 [aesonQQ| 2 |]
      assertInvalid schemaExclusiveMinimum3 [aesonQQ| 3 |]
      assertValid schemaExclusiveMinimum3 [aesonQQ| 4 |]
      let schemaMaximum3 = [aesonQQ| { "type": "number", "maximum": 3 } |]
      assertValid schemaMaximum3 [aesonQQ| 2 |]
      assertValid schemaMaximum3 [aesonQQ| 3 |]
      assertInvalid schemaMaximum3 [aesonQQ| 4 |]
      let schemaExclusiveMaximum3 = [aesonQQ| {
            "type": "number",
            "maximum": 3,
            "exclusiveMaximum": true
          }]
      assertValid schemaExclusiveMaximum3 [aesonQQ| 2 |]
      assertInvalid schemaExclusiveMaximum3 [aesonQQ| 3 |]
      assertInvalid schemaExclusiveMaximum3 [aesonQQ| 4 |]
  , testCase "divisibleBy" $ do
      let by2 = [aesonQQ| { "type": "number", "divisibleBy": 2 } |]
      assertValid by2 [aesonQQ| 2 |]
      assertValid by2 [aesonQQ| 4 |]
      assertValid by2 [aesonQQ| 0 |]
      assertInvalid by2 [aesonQQ| 1 |]
      assertInvalid by2 [aesonQQ| 3 |]
      let byOneAndHalf = [aesonQQ| { "type": "number", "divisibleBy": 1.5 }]
      assertValid byOneAndHalf [aesonQQ| 1.5 |]
      assertValid byOneAndHalf [aesonQQ| 3 |]
      assertValid byOneAndHalf [aesonQQ| 4.5 |]
      assertInvalid byOneAndHalf [aesonQQ| 2.5 |]
  , testCase "type: \"string\"" $ do
      let schema = [aesonQQ| { "type": "string" }]
      assertInvalid schema [aesonQQ| 3 |]
      assertInvalid schema [aesonQQ| true |]
      assertValid schema [aesonQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid schema [aesonQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonQQ| null |]
  , testCase "minLength and maxLength" $ do
      let minLength2 = [aesonQQ| { "type": "string", "minLength": 2 } |]
      assertInvalid minLength2 [aesonQQ| "" |]
      assertInvalid minLength2 [aesonQQ| "a" |]
      assertValid minLength2 [aesonQQ| "aa" |]
      let maxLength5 = [aesonQQ| { "type": "string", "maxLength": 5 } |]
      assertValid maxLength5 [aesonQQ| "" |]
      assertValid maxLength5 [aesonQQ| "lorem" |]
      assertInvalid maxLength5 [aesonQQ| "lorem ipsum" |]
  , testCase "pattern" $ do
      assertInvalid [aesonQQ| { "type": "string", "pattern": ".+" } |] ""
      assertValid [aesonQQ| { "type": "string", "pattern": ".+" } |] "one does not simply ..."
      assertValid [aesonQQ| { "type": "string", "pattern": "^([a-z][0-9])+$" } |] "h8w2o8e3"
      assertInvalid [aesonQQ| { "type": "string", "pattern": "^([a-z][0-9])+$" } |] "h8w2o8e35"
  , testCase "type: \"boolean\"" $ do
      let schema = [aesonQQ| { "type": "boolean" }]
      assertInvalid schema [aesonQQ| 3 |]
      assertValid schema [aesonQQ| true |]
      assertInvalid schema [aesonQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid schema [aesonQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonQQ| null |]
  , testCase "type: \"null\"" $ do
      let schema = [aesonQQ| { "type": "null" }]
      assertInvalid schema [aesonQQ| 3 |]
      assertInvalid schema [aesonQQ| true |]
      assertInvalid schema [aesonQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid schema [aesonQQ| ["eins", "zwei"] |]
      assertValid schema [aesonQQ| null |]
  , testCase "type: \"array\"" $ do
      let schema = [aesonQQ| { "type": "array" }]
      assertInvalid schema [aesonQQ| 3 |]
      assertInvalid schema [aesonQQ| true |]
      assertInvalid schema [aesonQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonQQ| { "eins": 1, "zwei": 2 } |]
      assertValid schema [aesonQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonQQ| null |]
  , testCase "minItems and maxItems" $ do
      let min3 = [aesonQQ| { "type": "array", "minItems": 3 } |]
      assertInvalid min3 [aesonQQ| [] |]
      assertInvalid min3 [aesonQQ| [1, true] |]
      assertValid min3 [aesonQQ| [1, true, null] |]
      let max3 = [aesonQQ| { "type": "array", "maxItems": 3 } |]
      assertValid max3 [aesonQQ| [] |]
      assertValid max3 [aesonQQ| [1, true, null] |]
      assertInvalid max3 [aesonQQ| [1, true, null, "lorem"] |]
  , testCase "uniqueItems" $ do
      let schema = [aesonQQ| { "type": "array", "uniqueItems": true } |]
      assertValid schema [aesonQQ| [] |]
      assertValid schema [aesonQQ| [1, 2] |]
      assertInvalid schema [aesonQQ| [1, 2, 1] |]
      assertValid schema [aesonQQ| [{ "lorem": "ipsum" }, 2, { "ipsum": "lorem" }] |]
      assertInvalid schema [aesonQQ| [{ "lorem": "ipsum" }, 2, { "lorem": "ipsum" }] |]
  , testCase "items" $ do
      let onlyNumbers = [aesonQQ| { "type": "array", "items": { "type": "number" } } |]
      assertValid onlyNumbers [aesonQQ| [1, 2, 3] |]
      assertInvalid onlyNumbers [aesonQQ| [1, 2, 3, "four"] |]
      let onlyStrings = [aesonQQ| { "type": "array", "items": { "type": "string" } } |]
      assertValid onlyStrings [aesonQQ| ["one", "two", "three"] |]
      assertInvalid onlyStrings [aesonQQ| ["one", "two", "three", 4] |]
      let stringNumberNull = [aesonQQ| {
            "type": "array",
            "items": [
              { "type": "string" },
              { "type": "number" },
              { "type": "null" }
            ]
          } |]
      assertValid stringNumberNull [aesonQQ| [] |]
      assertInvalid stringNumberNull [aesonQQ| [3] |]
      assertValid stringNumberNull [aesonQQ| ["lorem"] |]
      assertValid stringNumberNull [aesonQQ| ["lorem", 3] |]
      assertInvalid stringNumberNull [aesonQQ| ["ipsum", null] |]
      assertValid stringNumberNull [aesonQQ| ["lorem", 3, null] |]
      assertInvalid stringNumberNull [aesonQQ| ["lorem", 3, 4] |]
      assertValid stringNumberNull [aesonQQ| ["lorem", 3, null, 1, 2, 3] |]
  , testCase "additionalItems" $ do
      let allowed = [aesonQQ| {
            "type": "array",
            "items": [ { "type": "string" }, { "type": "number" } ],
            "additionalItems": true
          } |]
      assertValid allowed [aesonQQ| ["abc", 123] |]
      assertValid allowed [aesonQQ| ["abc", 123, [], null, true] |]
      let forbidden = [aesonQQ| {
            "type": "array",
            "items": [ { "type": "string" }, { "type": "number" } ],
            "additionalItems": false
          } |]
      assertValid forbidden [aesonQQ| ["abc", 123] |]
      assertInvalid forbidden [aesonQQ| ["abc", 123, [], null, true] |]
      let onlyNulls = [aesonQQ| {
            "type": "array",
            "items": [ { "type": "string" }, { "type": "number" } ],
            "additionalItems": { "type": "null" }
          } |]
      assertValid onlyNulls [aesonQQ| ["abc", 123] |]
      assertInvalid onlyNulls [aesonQQ| ["abc", 123, [], null, true] |]
      assertValid onlyNulls [aesonQQ| ["abc", 123, null, null, null, null] |]
  , testCase "type: \"object\"" $ do
      let schema = [aesonQQ| { "type": "object" }]
      assertInvalid schema [aesonQQ| 3 |]
      assertInvalid schema [aesonQQ| true |]
      assertInvalid schema [aesonQQ| "nobody expects the ..." |]
      assertValid schema [aesonQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid schema [aesonQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonQQ| null |]
  , testCase "properties" $ do
      let schema = [aesonQQ| {
            "type": "object",
            "properties": {
              "aNumber": { "type": "number" },
              "aString": { "type": "string" }
            }
          } |]
      assertValid schema emptyObject
      assertValid schema [aesonQQ| { "aNumber": 2 } |]
      assertValid schema [aesonQQ| { "aString": "fromage" } |]
      assertValid schema [aesonQQ| { "aNumber": 2, "aString": "fromage" } |]
      assertInvalid schema [aesonQQ| { "aNumber": "deux" } |]
      assertInvalid schema [aesonQQ| { "aString": 42 } |]
  , testCase "patternProperties" $ do
      let schema = [aesonQQ| {
            "type": "object",
            "properties": {
              "positiveNumber": {
                "type": "number",
                "minimum": 0,
                "exclusiveMinimum": true
              }
            },
            "patternProperties": {
              ".+Number$": { "type": "integer" },
              ".+String$": { "type": "string" }
            }
          } |]
      assertValid schema $ object [("positiveNumber", Number (fromInteger 13))]
      assertInvalid schema $ object [("positiveNumber", Number (fromInteger (-13)))]
      assertInvalid schema $ object [("positiveNumber", Number (fromRational (27 % 2)))]
      assertValid schema [aesonQQ| { "fooString": "foo", "barString": "bar" } |]
      assertInvalid schema [aesonQQ| { "fooString": null, "barString": "bar" } |]
  , testCase "additionalProperties" $ do
      let additionalNumbers = [aesonQQ| {
            "type": "object",
            "properties": { "null": { "type": "null" } },
            "patternProperties": {
              ".+String$": { "type": "string" }
            },
            "additionalProperties": { "type": "number" }
          } |]
      assertValid additionalNumbers [aesonQQ| {
        "null": null,
        "emptyString": "",
        "oneMoreThing": 23,
        "theLastThing": 999
      } |]
      assertInvalid additionalNumbers [aesonQQ| { "null": null, "notANumber": true } |]
      let noAdditionalProperties = [aesonQQ| {
            "type": "object",
            "properties": { "null": { "type": "null" } },
            "patternProperties": {
              ".+String$": { "type": "string" }
            },
            "additionalProperties": false
          } |]
      assertValid noAdditionalProperties [aesonQQ| { "null": null, "emptyString": "" } |]
      assertInvalid noAdditionalProperties [aesonQQ| {
        "null": null,
        "emptyString": "",
        "oneMoreThing": 23,
        "theLastThing": 999
      } |]
  , testCase "enum" $ do
      let testStrings = [aesonQQ| { "type": "string", "enum": ["foo", "bar", "blub"] } |]
      assertValid testStrings "foo"
      assertValid testStrings "bar"
      assertValid testStrings "blub"
      assertInvalid testStrings "lorem"
      let oneTwoMapping = [aesonQQ| {
            "type": "object",
            "enum": [{ "eins": 1, "zwei": 2 }, { "un": 1, "deux": 2 }]
          } |]
      assertValid oneTwoMapping [aesonQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid oneTwoMapping emptyObject
      assertInvalid oneTwoMapping [aesonQQ| { "eins": 1, "zwei": 2, "drei": 3 } |]
      assertValid oneTwoMapping [aesonQQ| { "un": 1, "deux": 2 } |]
      assertInvalid oneTwoMapping [aesonQQ| { "one": 1, "two": 2 } |]
  , testCase "type: \"any\"" $ do
      let schema = [aesonQQ| {
            "type": "any",
            "minimum": 2,
            "maxItems": 2,
            "pattern": "a$"
          } |]
      assertValid schema [aesonQQ| "a" |]
      assertInvalid schema [aesonQQ| "b" |]
      assertValid schema [aesonQQ| 3 |]
      assertInvalid schema [aesonQQ| 1 |]
      assertValid schema [aesonQQ| null |]
      assertValid schema [aesonQQ| true |]
      assertValid schema [aesonQQ| { "une": 1 } |]
      assertValid schema [aesonQQ| [true, false] |]
      assertInvalid schema [aesonQQ| [true, false, true] |]
  , testCase "disallow" $ do
      let onlyFloats = [aesonQQ| { "type": "number", "disallow": "integer" } |]
      assertInvalid onlyFloats (Number $ fromInteger 3)
      assertValid onlyFloats (Number $ 9 + fromRational (3 % 4))
      let notLengthThree = [aesonQQ| {
            "type": "array",
            "disallow": [{
              "type": "array",
              "minItems": 3,
              "maxItems": 3
            }]
          } |]
      assertValid notLengthThree [aesonQQ| [] |]
      assertValid notLengthThree [aesonQQ| [1] |]
      assertValid notLengthThree [aesonQQ| [1, 2] |]
      assertInvalid notLengthThree [aesonQQ| [1, 2, 3] |]
      assertValid notLengthThree [aesonQQ| [1, 2, 3, 4] |]
      let everythingExceptNumbers = [aesonQQ| { "disallow": "number" } |]
      assertInvalid everythingExceptNumbers [aesonQQ| 3 |]
      assertInvalid everythingExceptNumbers [aesonQQ| 3.5 |]
      assertValid everythingExceptNumbers [aesonQQ| true |]
      assertValid everythingExceptNumbers [aesonQQ| "nobody expects the ..." |]
      assertValid everythingExceptNumbers [aesonQQ| { eins: 1, zwei: 2 } |]
      assertValid everythingExceptNumbers [aesonQQ| ["eins", "zwei"] |]
      assertValid everythingExceptNumbers [aesonQQ| null |]
  , testGroup "format"
      [ testCase "regex" $ do
          assertValid [aesonQQ| { "type": "string", "format": "regex" } |] "([abc])+\\s+$"
          assertInvalid [aesonQQ| { "type": "string", "format": "regex" } |] "^(abc]"
      ]
  , testCase "type: subschema" $ do
      let schema = [aesonQQ| {
            "type": [
              {
                "type": "object",
                "properties": { "insert": { "type": "string", "minLength": 1 } },
                "additionalProperties": false
              },
              {
                "type": "object",
                "properties": { "delete": { "type": "number", "minimum": 1 } },
                "additionalProperties": false
              },
              {
                "type": "object",
                "properties": { "retain": { "type": "number", "minimum": 1 } },
                "additionalProperties": false
              }
            ]
          } |]
      assertValid schema [aesonQQ| { "insert": "lorem" } |]
      assertInvalid schema [aesonQQ| { "insert": "lorem", "delete": 5 } |]
      assertValid schema [aesonQQ| { "delete": 5 } |]
      assertInvalid schema [aesonQQ| { "delete": 5, "retain": 76 } |]
      assertValid schema [aesonQQ| { "retain": 76 } |]
  , testCase "dependencies" $ do
      let aRequiresB = [aesonQQ| {
            "type": "object",
            "dependencies": { "a": "b" }
          } |]
      assertValid aRequiresB emptyObject
      assertValid aRequiresB [aesonQQ| { "b": false } |]
      assertValid aRequiresB [aesonQQ| { "a": true, "b": false } |]
      assertInvalid aRequiresB [aesonQQ| { "a": 3 } |]
      let aRequiresBToBeANumber = [aesonQQ| {
            "type": "object",
            "dependencies": { "a": { "properties": { "b": { "type": "number" } } } }
          } |]
      assertValid aRequiresBToBeANumber emptyObject
      assertValid aRequiresBToBeANumber [aesonQQ| { "b": "lorem" } |]
      assertValid aRequiresBToBeANumber [aesonQQ| { "a": "yes, we can" } |]
      assertInvalid aRequiresBToBeANumber [aesonQQ| { "a": "yes, we can", "b": "lorem" } |]
      assertValid aRequiresBToBeANumber [aesonQQ| { "a": "hi there", "b": 42 } |]
  ]
  where
    assertValid, assertInvalid :: Value -> Value -> HU.Assertion
    assertValid sch inst = do
      schema <- parseSchema sch
      case validate schema inst of
        Just e -> HU.assertFailure e
        Nothing -> return ()
    assertInvalid sch inst = do
      schema <- parseSchema sch
      case validate schema inst of
        Just e -> return ()
        Nothing -> HU.assertFailure "expected a validation error"
    parseSchema :: Value -> IO (Schema String)
    parseSchema v = case fromJSON v of
      Error e -> HU.assertFailure e >> fail "invalid schema"
      Success schema -> return schema