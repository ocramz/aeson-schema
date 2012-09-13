{-# LANGUAGE QuasiQuotes #-}

module Data.Aeson.Schema.Examples
  ( examples
  ) where

import           Data.Aeson        (Value)
import           Data.Aeson.LitQQ  (aesonLitQQ)
import qualified Data.Map          as M
import           Data.Text         (Text)

import           Data.Aeson.Schema

examples :: (String -> IO () -> a)
         -> (Graph Schema Text -> Schema Text -> Value -> IO ())
         -> (Graph Schema Text -> Schema Text -> Value -> IO ())
         -> [a]
examples testCase assertValid' assertInvalid' =
  [ testCase "type: \"number\"" $ do
      let schema = [schemaQQ| { "type": "number" } |]
      assertValid schema [aesonLitQQ| 3 |]
      assertValid schema [aesonLitQQ| 3.5 |]
      assertInvalid schema [aesonLitQQ| true |]
      assertInvalid schema [aesonLitQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonLitQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid schema [aesonLitQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonLitQQ| null |]
  , testCase "type: \"integer\"" $ do
      let schema = [schemaQQ| { "type": "integer" } |]
      assertValid schema [aesonLitQQ| 3 |]
      assertInvalid schema [aesonLitQQ| 3.5 |]
      assertInvalid schema [aesonLitQQ| true |]
      assertInvalid schema [aesonLitQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonLitQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid schema [aesonLitQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonLitQQ| null |]
  , testCase "maximum and minimum" $ do
      let schemaMinimum3 = [schemaQQ| { "type": "number", "minimum": 3 } |]
      assertInvalid schemaMinimum3 [aesonLitQQ| 2 |]
      assertValid schemaMinimum3 [aesonLitQQ| 3 |]
      assertValid schemaMinimum3 [aesonLitQQ| 4 |]
      let schemaExclusiveMinimum3 = [schemaQQ| {
        "type": "number",
        "minimum": 3,
        "exclusiveMinimum": true
      } |]
      assertInvalid schemaExclusiveMinimum3 [aesonLitQQ| 2 |]
      assertInvalid schemaExclusiveMinimum3 [aesonLitQQ| 3 |]
      assertValid schemaExclusiveMinimum3 [aesonLitQQ| 4 |]
      let schemaMaximum3 = [schemaQQ| { "type": "number", "maximum": 3 } |]
      assertValid schemaMaximum3 [aesonLitQQ| 2 |]
      assertValid schemaMaximum3 [aesonLitQQ| 3 |]
      assertInvalid schemaMaximum3 [aesonLitQQ| 4 |]
      let schemaExclusiveMaximum3 = [schemaQQ| {
        "type": "number",
        "maximum": 3,
        "exclusiveMaximum": true
      } |]
      assertValid schemaExclusiveMaximum3 [aesonLitQQ| 2 |]
      assertInvalid schemaExclusiveMaximum3 [aesonLitQQ| 3 |]
      assertInvalid schemaExclusiveMaximum3 [aesonLitQQ| 4 |]
  , testCase "divisibleBy" $ do
      let by2 = [schemaQQ| { "type": "number", "divisibleBy": 2 } |]
      assertValid by2 [aesonLitQQ| 2 |]
      assertValid by2 [aesonLitQQ| 4 |]
      assertValid by2 [aesonLitQQ| 0 |]
      assertInvalid by2 [aesonLitQQ| 1 |]
      assertInvalid by2 [aesonLitQQ| 3 |]
      let byOneAndHalf = [schemaQQ| { "type": "number", "divisibleBy": 1.5 }]
      assertValid byOneAndHalf [aesonLitQQ| 1.5 |]
      assertValid byOneAndHalf [aesonLitQQ| 3 |]
      assertValid byOneAndHalf [aesonLitQQ| 4.5 |]
      assertInvalid byOneAndHalf [aesonLitQQ| 2.5 |]
  , testCase "type: \"string\"" $ do
      let schema = [schemaQQ| { "type": "string" } |]
      assertInvalid schema [aesonLitQQ| 3 |]
      assertInvalid schema [aesonLitQQ| true |]
      assertValid schema [aesonLitQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonLitQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid schema [aesonLitQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonLitQQ| null |]
  , testCase "minLength and maxLength" $ do
      let minLength2 = [schemaQQ| { "type": "string", "minLength": 2 } |]
      assertInvalid minLength2 [aesonLitQQ| "" |]
      assertInvalid minLength2 [aesonLitQQ| "a" |]
      assertValid minLength2 [aesonLitQQ| "aa" |]
      let maxLength5 = [schemaQQ| { "type": "string", "maxLength": 5 } |]
      assertValid maxLength5 [aesonLitQQ| "" |]
      assertValid maxLength5 [aesonLitQQ| "lorem" |]
      assertInvalid maxLength5 [aesonLitQQ| "lorem ipsum" |]
  , testCase "pattern" $ do
      let notEmptyString = [schemaQQ| { "type": "string", "pattern": ".+" } |]
      assertInvalid notEmptyString ""
      assertValid notEmptyString "one does not simply ..."
      let letterDigitAlternating = [schemaQQ| { "type": "string", "pattern": "^([a-z][0-9])+$" } |]
      assertValid letterDigitAlternating "h8w2o8e3"
      assertInvalid letterDigitAlternating "h8w2o8e35"
  , testCase "type: \"boolean\"" $ do
      let schema = [schemaQQ| { "type": "boolean" } |]
      assertInvalid schema [aesonLitQQ| 3 |]
      assertValid schema [aesonLitQQ| true |]
      assertInvalid schema [aesonLitQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonLitQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid schema [aesonLitQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonLitQQ| null |]
  , testCase "type: \"null\"" $ do
      let schema = [schemaQQ| { "type": "null" } |]
      assertInvalid schema [aesonLitQQ| 3 |]
      assertInvalid schema [aesonLitQQ| true |]
      assertInvalid schema [aesonLitQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonLitQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid schema [aesonLitQQ| ["eins", "zwei"] |]
      assertValid schema [aesonLitQQ| null |]
  , testCase "type: \"array\"" $ do
      let schema = [schemaQQ| { "type": "array" } |]
      assertInvalid schema [aesonLitQQ| 3 |]
      assertInvalid schema [aesonLitQQ| true |]
      assertInvalid schema [aesonLitQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonLitQQ| { "eins": 1, "zwei": 2 } |]
      assertValid schema [aesonLitQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonLitQQ| null |]
  , testCase "minItems and maxItems" $ do
      let min3 = [schemaQQ| { "type": "array", "minItems": 3 } |]
      assertInvalid min3 [aesonLitQQ| [] |]
      assertInvalid min3 [aesonLitQQ| [1, true] |]
      assertValid min3 [aesonLitQQ| [1, true, null] |]
      let max3 = [schemaQQ| { "type": "array", "maxItems": 3 } |]
      assertValid max3 [aesonLitQQ| [] |]
      assertValid max3 [aesonLitQQ| [1, true, null] |]
      assertInvalid max3 [aesonLitQQ| [1, true, null, "lorem"] |]
  , testCase "uniqueItems" $ do
      let schema = [schemaQQ| { "type": "array", "uniqueItems": true } |]
      assertValid schema [aesonLitQQ| [] |]
      assertValid schema [aesonLitQQ| [1, 2] |]
      assertInvalid schema [aesonLitQQ| [1, 2, 1] |]
      assertValid schema [aesonLitQQ| [{ "lorem": "ipsum" }, 2, { "ipsum": "lorem" }] |]
      assertInvalid schema [aesonLitQQ| [{ "lorem": "ipsum" }, 2, { "lorem": "ipsum" }] |]
  , testCase "items" $ do
      let onlyNumbers = [schemaQQ| { "type": "array", "items": { "type": "number" } } |]
      assertValid onlyNumbers [aesonLitQQ| [1, 2, 3] |]
      assertInvalid onlyNumbers [aesonLitQQ| [1, 2, 3, "four"] |]
      let onlyStrings = [schemaQQ| { "type": "array", "items": { "type": "string" } } |]
      assertValid onlyStrings [aesonLitQQ| ["one", "two", "three"] |]
      assertInvalid onlyStrings [aesonLitQQ| ["one", "two", "three", 4] |]
      let stringNumberNull = [schemaQQ| {
        "type": "array",
        "items": [
          { "type": "string" },
          { "type": "number" },
          { "type": "null" }
        ]
      } |]
      assertValid stringNumberNull [aesonLitQQ| [] |]
      assertInvalid stringNumberNull [aesonLitQQ| [3] |]
      assertValid stringNumberNull [aesonLitQQ| ["lorem"] |]
      assertValid stringNumberNull [aesonLitQQ| ["lorem", 3] |]
      assertInvalid stringNumberNull [aesonLitQQ| ["ipsum", null] |]
      assertValid stringNumberNull [aesonLitQQ| ["lorem", 3, null] |]
      assertInvalid stringNumberNull [aesonLitQQ| ["lorem", 3, 4] |]
      assertValid stringNumberNull [aesonLitQQ| ["lorem", 3, null, 1, 2, 3] |]
  , testCase "additionalItems" $ do
      let allowed = [schemaQQ| {
        "type": "array",
        "items": [ { "type": "string" }, { "type": "number" } ],
        "additionalItems": true
      } |]
      assertValid allowed [aesonLitQQ| ["abc", 123] |]
      assertValid allowed [aesonLitQQ| ["abc", 123, [], null, true] |]
      let forbidden = [schemaQQ| {
        "type": "array",
        "items": [ { "type": "string" }, { "type": "number" } ],
        "additionalItems": false
      } |]
      assertValid forbidden [aesonLitQQ| ["abc", 123] |]
      assertInvalid forbidden [aesonLitQQ| ["abc", 123, [], null, true] |]
      let onlyNulls = [schemaQQ| {
        "type": "array",
        "items": [ { "type": "string" }, { "type": "number" } ],
        "additionalItems": { "type": "null" }
      } |]
      assertValid onlyNulls [aesonLitQQ| ["abc", 123] |]
      assertInvalid onlyNulls [aesonLitQQ| ["abc", 123, [], null, true] |]
      assertValid onlyNulls [aesonLitQQ| ["abc", 123, null, null, null, null] |]
  , testCase "type: \"object\"" $ do
      let schema = [schemaQQ| { "type": "object" } |]
      assertInvalid schema [aesonLitQQ| 3 |]
      assertInvalid schema [aesonLitQQ| true |]
      assertInvalid schema [aesonLitQQ| "nobody expects the ..." |]
      assertValid schema [aesonLitQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid schema [aesonLitQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonLitQQ| null |]
  , testCase "properties" $ do
      let schema = [schemaQQ| {
        "type": "object",
        "properties": {
          "aNumber": { "type": "number" },
          "aString": { "type": "string" }
        }
      } |]
      assertValid schema [aesonLitQQ| {} |]
      assertValid schema [aesonLitQQ| { "aNumber": 2 } |]
      assertValid schema [aesonLitQQ| { "aString": "fromage" } |]
      assertValid schema [aesonLitQQ| { "aNumber": 2, "aString": "fromage" } |]
      assertInvalid schema [aesonLitQQ| { "aNumber": "deux" } |]
      assertInvalid schema [aesonLitQQ| { "aString": 42 } |]
  , testCase "patternProperties" $ do
      let schema = [schemaQQ| {
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
      assertValid schema [aesonLitQQ| { "positiveNumber": 13 } |]
      assertInvalid schema [aesonLitQQ| { "positiveNumber": -13 } |]
      assertInvalid schema [aesonLitQQ| { "positiveNumber": 13.5 } |]
      assertValid schema [aesonLitQQ| { "fooString": "foo", "barString": "bar" } |]
      assertInvalid schema [aesonLitQQ| { "fooString": null, "barString": "bar" } |]
  , testCase "additionalProperties" $ do
      let additionalNumbers = [schemaQQ| {
        "type": "object",
        "properties": { "null": { "type": "null" } },
        "patternProperties": {
          ".+String$": { "type": "string" }
        },
        "additionalProperties": { "type": "number" }
      } |]
      assertValid additionalNumbers [aesonLitQQ| {
        "null": null,
        "emptyString": "",
        "oneMoreThing": 23,
        "theLastThing": 999
      } |]
      assertInvalid additionalNumbers [aesonLitQQ| { "null": null, "notANumber": true } |]
      let noAdditionalProperties = [schemaQQ| {
        "type": "object",
        "properties": { "null": { "type": "null" } },
        "patternProperties": {
          ".+String$": { "type": "string" }
        },
        "additionalProperties": false
      } |]
      assertValid noAdditionalProperties [aesonLitQQ| { "null": null, "emptyString": "" } |]
      assertInvalid noAdditionalProperties [aesonLitQQ| {
        "null": null,
        "emptyString": "",
        "oneMoreThing": 23,
        "theLastThing": 999
      } |]
  , testCase "enum" $ do
      let testStrings = [schemaQQ| { "type": "string", "enum": ["foo", "bar", "blub"] } |]
      assertValid testStrings "foo"
      assertValid testStrings "bar"
      assertValid testStrings "blub"
      assertInvalid testStrings "lorem"
      let oneTwoMapping = [schemaQQ| {
        "type": "object",
        "enum": [{ "eins": 1, "zwei": 2 }, { "un": 1, "deux": 2 }]
      } |]
      assertValid oneTwoMapping [aesonLitQQ| { "eins": 1, "zwei": 2 } |]
      assertInvalid oneTwoMapping [aesonLitQQ| {} |]
      assertInvalid oneTwoMapping [aesonLitQQ| { "eins": 1, "zwei": 2, "drei": 3 } |]
      assertValid oneTwoMapping [aesonLitQQ| { "un": 1, "deux": 2 } |]
      assertInvalid oneTwoMapping [aesonLitQQ| { "one": 1, "two": 2 } |]
  , testCase "type: \"any\"" $ do
      let schema = [schemaQQ| {
        "type": "any",
        "minimum": 2,
        "maxItems": 2,
        "pattern": "a$"
      } |]
      assertValid schema [aesonLitQQ| "a" |]
      assertInvalid schema [aesonLitQQ| "b" |]
      assertValid schema [aesonLitQQ| 3 |]
      assertInvalid schema [aesonLitQQ| 1 |]
      assertValid schema [aesonLitQQ| null |]
      assertValid schema [aesonLitQQ| true |]
      assertValid schema [aesonLitQQ| { "une": 1 } |]
      assertValid schema [aesonLitQQ| [true, false] |]
      assertInvalid schema [aesonLitQQ| [true, false, true] |]
  , testCase "disallow" $ do
      let onlyFloats = [schemaQQ| { "type": "number", "disallow": "integer" } |]
      assertInvalid onlyFloats [aesonLitQQ| 9 |]
      assertValid onlyFloats [aesonLitQQ| 9.75 |]
      let notLengthThree = [schemaQQ| {
        "type": "array",
        "disallow": [{
          "type": "array",
          "minItems": 3,
          "maxItems": 3
        }]
      } |]
      assertValid notLengthThree [aesonLitQQ| [] |]
      assertValid notLengthThree [aesonLitQQ| [1] |]
      assertValid notLengthThree [aesonLitQQ| [1, 2] |]
      assertInvalid notLengthThree [aesonLitQQ| [1, 2, 3] |]
      assertValid notLengthThree [aesonLitQQ| [1, 2, 3, 4] |]
      let everythingExceptNumbers = [schemaQQ| { "disallow": "number" } |]
      assertInvalid everythingExceptNumbers [aesonLitQQ| 3 |]
      assertInvalid everythingExceptNumbers [aesonLitQQ| 3.5 |]
      assertValid everythingExceptNumbers [aesonLitQQ| true |]
      assertValid everythingExceptNumbers [aesonLitQQ| "nobody expects the ..." |]
      assertValid everythingExceptNumbers [aesonLitQQ| { "eins": 1, "zwei": 2 } |]
      assertValid everythingExceptNumbers [aesonLitQQ| ["eins", "zwei"] |]
      assertValid everythingExceptNumbers [aesonLitQQ| null |]
  , testCase "format: \"regex\"" $ do
      let isRegex = [schemaQQ| { "type": "string", "format": "regex" } |]
      assertValid isRegex "([abc])+\\s+$"
      assertInvalid isRegex "^(abc]"
  , testCase "type: subschema" $ do
      let schema = [schemaQQ| {
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
      assertValid schema [aesonLitQQ| { "insert": "lorem" } |]
      assertInvalid schema [aesonLitQQ| { "insert": "lorem", "delete": 5 } |]
      assertValid schema [aesonLitQQ| { "delete": 5 } |]
      assertInvalid schema [aesonLitQQ| { "delete": 5, "retain": 76 } |]
      assertValid schema [aesonLitQQ| { "retain": 76 } |]
  , testCase "dependencies" $ do
      let aRequiresB = [schemaQQ| {
        "type": "object",
        "dependencies": { "a": "b" }
      } |]
      assertValid aRequiresB [aesonLitQQ| {} |]
      assertValid aRequiresB [aesonLitQQ| { "b": false } |]
      assertValid aRequiresB [aesonLitQQ| { "a": true, "b": false } |]
      assertInvalid aRequiresB [aesonLitQQ| { "a": 3 } |]
      let aRequiresBToBeANumber = [schemaQQ| {
        "type": "object",
        "dependencies": { "a": { "properties": { "b": { "type": "number" } } } }
      } |]
      assertValid aRequiresBToBeANumber [aesonLitQQ| {} |]
      assertValid aRequiresBToBeANumber [aesonLitQQ| { "b": "lorem" } |]
      assertValid aRequiresBToBeANumber [aesonLitQQ| { "a": "yes, we can" } |]
      assertInvalid aRequiresBToBeANumber [aesonLitQQ| { "a": "yes, we can", "b": "lorem" } |]
      assertValid aRequiresBToBeANumber [aesonLitQQ| { "a": "hi there", "b": 42 } |]
      let aDisallowsB = [schemaQQ| {
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
      assertValid aDisallowsB [aesonLitQQ| { "a": "lorem" } |]
      assertValid aDisallowsB [aesonLitQQ| { "b": 42 } |]
      assertInvalid aDisallowsB [aesonLitQQ| { "a": "lorem", "b": 42 } |]
  , testCase "required" $ do
      let schema = [schemaQQ| {
        "type": "object",
        "properties": {
          "a": { "required": true }
        }
      } |]
      assertInvalid schema [aesonLitQQ| {} |]
      assertValid schema [aesonLitQQ| { "a": [1, 2, 3] } |]
  , testCase "extends" $ do
      let schema = [schemaQQ| {
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
      assertValid schema [aesonLitQQ| { "a": 2 } |]
      assertInvalid schema [aesonLitQQ| {} |]
      assertInvalid schema [aesonLitQQ| { "a": -4 } |]
      assertInvalid schema [aesonLitQQ| { "a": "foo" } |]
      assertInvalid schema [aesonLitQQ| { "a": -1, "b": -10 } |]
      assertValid schema [aesonLitQQ| { "a": -1, "ba": -10 } |]
  , testCase "$ref" $ do
      let
        a = [schemaQQ| { "$ref": "b", "minimum": 3 } |]
        b = [schemaQQ| { "type": "number", "maximum": 2 } |]
        graph = M.fromList [("a", a), ("b", b)]
      assertValid' graph a [aesonLitQQ| 1 |]
      assertInvalid' graph a [aesonLitQQ| 4 |]
  ]
  where
    assertValid = assertValid' M.empty
    assertInvalid = assertInvalid' M.empty
