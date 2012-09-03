module Data.Aeson.Schema.Validator.Tests
  ( tests
  ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HU

import Data.Aeson
import Data.Aeson.QQ
import Data.Aeson.Types (emptyObject)
import Data.Ratio ((%))
import qualified Data.Aeson.Types
import qualified Data.Attoparsec.Number
import qualified Data.Vector
import Data.Text (Text)
import qualified Data.Text
import qualified Data.Map as M

import Data.Aeson.Schema
import Data.Aeson.Schema.Validator
import Data.Aeson.Schema.Choice

assertValid, assertInvalid :: Schema V3 Text -> Value -> HU.Assertion
assertValid sch inst = case validate M.empty sch inst of
  Just e -> HU.assertFailure e
  Nothing -> return ()
assertInvalid sch inst = case validate M.empty sch inst of
  Just _ -> return ()
  Nothing -> HU.assertFailure "expected a validation error"

parseSchema :: Value -> IO (Schema V3 Text)
parseSchema v = case fromJSON v :: Result (Schema V3 String) of
  Error e -> HU.assertFailure e >> fail "invalid schema"
  Success schema -> return $ fmap (error "schema mustn't contain a $ref") schema

tests :: [Test]
tests =
  [ testCase "type: \"number\"" $ do
      schema <- parseSchema [aesonQQ| { "type": "number" } |]
      assertValid schema [aesonQQ| 3 |]
      assertValid schema [aesonQQ| 3.5 |]
      assertInvalid schema [aesonQQ| true |]
      assertInvalid schema [aesonQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonQQ| { eins: 1, zwei: 2 } |]
      assertInvalid schema [aesonQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonQQ| null |]
  , testCase "type: \"integer\"" $ do
      schema <- parseSchema [aesonQQ| { "type": "integer" } |]
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
      schemaMinimum3 <- parseSchema [aesonQQ| { "type": "number", "minimum": 3 } |]
      assertInvalid schemaMinimum3 [aesonQQ| 2 |]
      assertValid schemaMinimum3 [aesonQQ| 3 |]
      assertValid schemaMinimum3 [aesonQQ| 4 |]
      schemaExclusiveMinimum3 <- parseSchema [aesonQQ| {
        "type": "number",
        "minimum": 3,
        "exclusiveMinimum": true
      }]
      assertInvalid schemaExclusiveMinimum3 [aesonQQ| 2 |]
      assertInvalid schemaExclusiveMinimum3 [aesonQQ| 3 |]
      assertValid schemaExclusiveMinimum3 [aesonQQ| 4 |]
      schemaMaximum3 <- parseSchema [aesonQQ| { "type": "number", "maximum": 3 } |]
      assertValid schemaMaximum3 [aesonQQ| 2 |]
      assertValid schemaMaximum3 [aesonQQ| 3 |]
      assertInvalid schemaMaximum3 [aesonQQ| 4 |]
      schemaExclusiveMaximum3 <- parseSchema [aesonQQ| {
        "type": "number",
        "maximum": 3,
        "exclusiveMaximum": true
      }]
      assertValid schemaExclusiveMaximum3 [aesonQQ| 2 |]
      assertInvalid schemaExclusiveMaximum3 [aesonQQ| 3 |]
      assertInvalid schemaExclusiveMaximum3 [aesonQQ| 4 |]
  , testCase "divisibleBy" $ do
      by2 <- parseSchema [aesonQQ| { "type": "number", "divisibleBy": 2 } |]
      assertValid by2 [aesonQQ| 2 |]
      assertValid by2 [aesonQQ| 4 |]
      assertValid by2 [aesonQQ| 0 |]
      assertInvalid by2 [aesonQQ| 1 |]
      assertInvalid by2 [aesonQQ| 3 |]
      byOneAndHalf <- parseSchema [aesonQQ| { "type": "number", "divisibleBy": 1.5 }]
      assertValid byOneAndHalf [aesonQQ| 1.5 |]
      assertValid byOneAndHalf [aesonQQ| 3 |]
      assertValid byOneAndHalf [aesonQQ| 4.5 |]
      assertInvalid byOneAndHalf [aesonQQ| 2.5 |]
  , testCase "type: \"string\"" $ do
      schema <- parseSchema [aesonQQ| { "type": "string" }]
      assertInvalid schema [aesonQQ| 3 |]
      assertInvalid schema [aesonQQ| true |]
      assertValid schema [aesonQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid schema [aesonQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonQQ| null |]
  , testCase "minLength and maxLength" $ do
      minLength2 <- parseSchema [aesonQQ| { "type": "string", "minLength": 2 } |]
      assertInvalid minLength2 [aesonQQ| "" |]
      assertInvalid minLength2 [aesonQQ| "a" |]
      assertValid minLength2 [aesonQQ| "aa" |]
      maxLength5 <- parseSchema [aesonQQ| { "type": "string", "maxLength": 5 } |]
      assertValid maxLength5 [aesonQQ| "" |]
      assertValid maxLength5 [aesonQQ| "lorem" |]
      assertInvalid maxLength5 [aesonQQ| "lorem ipsum" |]
  , testCase "pattern" $ do
      notEmptyString <- parseSchema [aesonQQ| { "type": "string", "pattern": ".+" } |]
      assertInvalid notEmptyString ""
      assertValid notEmptyString "one does not simply ..."
      letterDigitAlternating <- parseSchema [aesonQQ| { "type": "string", "pattern": "^([a-z][0-9])+$" } |]
      assertValid letterDigitAlternating "h8w2o8e3"
      assertInvalid letterDigitAlternating "h8w2o8e35"
  , testCase "type: \"boolean\"" $ do
      schema <- parseSchema [aesonQQ| { "type": "boolean" }]
      assertInvalid schema [aesonQQ| 3 |]
      assertValid schema [aesonQQ| true |]
      assertInvalid schema [aesonQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid schema [aesonQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonQQ| null |]
  , testCase "type: \"null\"" $ do
      schema <- parseSchema [aesonQQ| { "type": "null" }]
      assertInvalid schema [aesonQQ| 3 |]
      assertInvalid schema [aesonQQ| true |]
      assertInvalid schema [aesonQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid schema [aesonQQ| ["eins", "zwei"] |]
      assertValid schema [aesonQQ| null |]
  , testCase "type: \"array\"" $ do
      schema <- parseSchema [aesonQQ| { "type": "array" }]
      assertInvalid schema [aesonQQ| 3 |]
      assertInvalid schema [aesonQQ| true |]
      assertInvalid schema [aesonQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonQQ| { "eins": 1, "zwei": 2 } |]
      assertValid schema [aesonQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonQQ| null |]
  , testCase "minItems and maxItems" $ do
      min3 <- parseSchema [aesonQQ| { "type": "array", "minItems": 3 } |]
      assertInvalid min3 [aesonQQ| [] |]
      assertInvalid min3 [aesonQQ| [1, true] |]
      assertValid min3 [aesonQQ| [1, true, null] |]
      max3 <- parseSchema [aesonQQ| { "type": "array", "maxItems": 3 } |]
      assertValid max3 [aesonQQ| [] |]
      assertValid max3 [aesonQQ| [1, true, null] |]
      assertInvalid max3 [aesonQQ| [1, true, null, "lorem"] |]
  , testCase "uniqueItems" $ do
      schema <- parseSchema [aesonQQ| { "type": "array", "uniqueItems": true } |]
      assertValid schema [aesonQQ| [] |]
      assertValid schema [aesonQQ| [1, 2] |]
      assertInvalid schema [aesonQQ| [1, 2, 1] |]
      assertValid schema [aesonQQ| [{ "lorem": "ipsum" }, 2, { "ipsum": "lorem" }] |]
      assertInvalid schema [aesonQQ| [{ "lorem": "ipsum" }, 2, { "lorem": "ipsum" }] |]
  , testCase "items" $ do
      onlyNumbers <- parseSchema [aesonQQ| { "type": "array", "items": { "type": "number" } } |]
      assertValid onlyNumbers [aesonQQ| [1, 2, 3] |]
      assertInvalid onlyNumbers [aesonQQ| [1, 2, 3, "four"] |]
      onlyStrings <- parseSchema [aesonQQ| { "type": "array", "items": { "type": "string" } } |]
      assertValid onlyStrings [aesonQQ| ["one", "two", "three"] |]
      assertInvalid onlyStrings [aesonQQ| ["one", "two", "three", 4] |]
      stringNumberNull <- parseSchema [aesonQQ| {
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
      allowed <- parseSchema [aesonQQ| {
        "type": "array",
        "items": [ { "type": "string" }, { "type": "number" } ],
        "additionalItems": true
      } |]
      assertValid allowed [aesonQQ| ["abc", 123] |]
      assertValid allowed [aesonQQ| ["abc", 123, [], null, true] |]
      forbidden <- parseSchema [aesonQQ| {
        "type": "array",
        "items": [ { "type": "string" }, { "type": "number" } ],
        "additionalItems": false
      } |]
      assertValid forbidden [aesonQQ| ["abc", 123] |]
      assertInvalid forbidden [aesonQQ| ["abc", 123, [], null, true] |]
      onlyNulls <- parseSchema [aesonQQ| {
        "type": "array",
        "items": [ { "type": "string" }, { "type": "number" } ],
        "additionalItems": { "type": "null" }
      } |]
      assertValid onlyNulls [aesonQQ| ["abc", 123] |]
      assertInvalid onlyNulls [aesonQQ| ["abc", 123, [], null, true] |]
      assertValid onlyNulls [aesonQQ| ["abc", 123, null, null, null, null] |]
  , testCase "type: \"object\"" $ do
      schema <- parseSchema [aesonQQ| { "type": "object" }]
      assertInvalid schema [aesonQQ| 3 |]
      assertInvalid schema [aesonQQ| true |]
      assertInvalid schema [aesonQQ| "nobody expects the ..." |]
      assertValid schema [aesonQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid schema [aesonQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonQQ| null |]
  , testCase "properties" $ do
      schema <- parseSchema [aesonQQ| {
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
      schema <- parseSchema [aesonQQ| {
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
      additionalNumbers <- parseSchema [aesonQQ| {
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
      noAdditionalProperties <- parseSchema [aesonQQ| {
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
      testStrings <- parseSchema [aesonQQ| { "type": "string", "enum": ["foo", "bar", "blub"] } |]
      assertValid testStrings "foo"
      assertValid testStrings "bar"
      assertValid testStrings "blub"
      assertInvalid testStrings "lorem"
      oneTwoMapping <- parseSchema [aesonQQ| {
        "type": "object",
        "enum": [{ "eins": 1, "zwei": 2 }, { "un": 1, "deux": 2 }]
      } |]
      assertValid oneTwoMapping [aesonQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid oneTwoMapping emptyObject
      assertInvalid oneTwoMapping [aesonQQ| { "eins": 1, "zwei": 2, "drei": 3 } |]
      assertValid oneTwoMapping [aesonQQ| { "un": 1, "deux": 2 } |]
      assertInvalid oneTwoMapping [aesonQQ| { "one": 1, "two": 2 } |]
  , testCase "type: \"any\"" $ do
      schema <- parseSchema [aesonQQ| {
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
      onlyFloats <- parseSchema [aesonQQ| { "type": "number", "disallow": "integer" } |]
      assertInvalid onlyFloats (Number $ fromInteger 3)
      assertValid onlyFloats (Number $ 9 + fromRational (3 % 4))
      notLengthThree <- parseSchema [aesonQQ| {
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
      everythingExceptNumbers <- parseSchema [aesonQQ| { "disallow": "number" } |]
      assertInvalid everythingExceptNumbers [aesonQQ| 3 |]
      assertInvalid everythingExceptNumbers [aesonQQ| 3.5 |]
      assertValid everythingExceptNumbers [aesonQQ| true |]
      assertValid everythingExceptNumbers [aesonQQ| "nobody expects the ..." |]
      assertValid everythingExceptNumbers [aesonQQ| { eins: 1, zwei: 2 } |]
      assertValid everythingExceptNumbers [aesonQQ| ["eins", "zwei"] |]
      assertValid everythingExceptNumbers [aesonQQ| null |]
  , testGroup "format"
      [ testCase "regex" $ do
          isRegex <- parseSchema [aesonQQ| { "type": "string", "format": "regex" } |]
          assertValid isRegex "([abc])+\\s+$"
          assertInvalid isRegex "^(abc]"
      ]
  , testCase "type: subschema" $ do
      schema <- parseSchema [aesonQQ| {
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
      aRequiresB <- parseSchema [aesonQQ| {
        "type": "object",
        "dependencies": { "a": "b" }
      } |]
      assertValid aRequiresB emptyObject
      assertValid aRequiresB [aesonQQ| { "b": false } |]
      assertValid aRequiresB [aesonQQ| { "a": true, "b": false } |]
      assertInvalid aRequiresB [aesonQQ| { "a": 3 } |]
      aRequiresBToBeANumber <- parseSchema [aesonQQ| {
        "type": "object",
        "dependencies": { "a": { "properties": { "b": { "type": "number" } } } }
      } |]
      assertValid aRequiresBToBeANumber emptyObject
      assertValid aRequiresBToBeANumber [aesonQQ| { "b": "lorem" } |]
      assertValid aRequiresBToBeANumber [aesonQQ| { "a": "yes, we can" } |]
      assertInvalid aRequiresBToBeANumber [aesonQQ| { "a": "yes, we can", "b": "lorem" } |]
      assertValid aRequiresBToBeANumber [aesonQQ| { "a": "hi there", "b": 42 } |]
      aDisallowsB <- parseSchema [aesonQQ| {
        "type": "object",
        "dependencies": {
          "a": {
            "disallow": [{
              "properties": {
                "b": { "type": "any", "required": true }
              }
            }]
          }
        }
      } |]
      assertValid aDisallowsB [aesonQQ| { "a": "lorem" } |]
      assertValid aDisallowsB [aesonQQ| { "b": 42 } |]
      assertInvalid aDisallowsB [aesonQQ| { "a": "lorem", "b": 42 } |]
  , testCase "required" $ do
      schema <- parseSchema [aesonQQ| {
        "type": "object",
        "properties": {
          "a": { "required": true }
        }
      } |]
      assertInvalid schema emptyObject
      assertValid schema [aesonQQ| { "a": [1, 2, 3] } |]
  , testCase "extends" $ do
      schema <- parseSchema [aesonQQ| {
        "type": "object",
        "properties": {
          "a": { "type": "number" }
        },
        "extends": [
          {
            "properties": {
              "a": { "required": true }
            }
          },
          {
            "patternProperties": {
              "^[a-z]$": { "minimum": -3 }
            }
          }
        ]
      } |]
      assertValid schema [aesonQQ| { "a": 2 } |]
      assertInvalid schema emptyObject
      assertInvalid schema [aesonQQ| { "a": -4 } |]
      assertInvalid schema [aesonQQ| { "a": "foo" } |]
      assertInvalid schema [aesonQQ| { "a": -1, "b": -10 } |]
      assertValid schema [aesonQQ| { "a": -1, "ba": -10 } |]
  , testCase "$ref" $ do
      let a = empty { schemaDRef = Just "b", schemaMinimum = Just 3 }
          b = empty { schemaType = [Choice1of2 NumberType], schemaMaximum = Just 2 }
          graph = M.fromList [("a" :: String, a), ("b", b)]
      case validate graph a (Number 1) of
        Nothing -> return ()
        Just e -> HU.assertFailure e
      case validate graph a (Number 4) of
        Nothing -> HU.assertFailure "4 should be invalid"
        Just _ -> return ()
  ]
