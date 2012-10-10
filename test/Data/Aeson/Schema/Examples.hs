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
  [ testCase "patternProperties" $ do
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
