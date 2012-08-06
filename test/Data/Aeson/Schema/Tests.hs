{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Data.Aeson.Schema.Tests
  ( tests
  ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HU

import Data.Foldable (toList)
import Data.Aeson
import qualified Data.Aeson.Types
import qualified Data.Attoparsec.Number
import qualified Data.Vector
import Data.Aeson.QQ
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
            , schemaPatternProperties = H.fromList [("lorem.+", schemaWithRef "c")]
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
      assertInvalid schema [aesonQQ| { eins: 1, zwei: 2 } |]
      assertInvalid schema [aesonQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonQQ| null |]
  , testCase "maximum and minimum" $ do
      let schemaMinimum3 = [aesonQQ| { "type": "number", "minimum": 3 } |]
      assertInvalid schemaMinimum3 [aesonQQ| 2 |]
      assertValid schemaMinimum3 [aesonQQ| 3 |]
      assertValid schemaMinimum3 [aesonQQ| 4 |]
      let schemaExclusiveMinimum3 = [aesonQQ| { "type": "number", "minimum": 3, "exclusiveMinimum": true }]
      assertInvalid schemaExclusiveMinimum3 [aesonQQ| 2 |]
      assertInvalid schemaExclusiveMinimum3 [aesonQQ| 3 |]
      assertValid schemaExclusiveMinimum3 [aesonQQ| 4 |]
      let schemaMaximum3 = [aesonQQ| { "type": "number", "maximum": 3 } |]
      assertValid schemaMaximum3 [aesonQQ| 2 |]
      assertValid schemaMaximum3 [aesonQQ| 3 |]
      assertInvalid schemaMaximum3 [aesonQQ| 4 |]
      let schemaExclusiveMaximum3 = [aesonQQ| { "type": "number", "maximum": 3, "exclusiveMaximum": true }]
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
      assertInvalid schema [aesonQQ| { eins: 1, zwei: 2 } |]
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