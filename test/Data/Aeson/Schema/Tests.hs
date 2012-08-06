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
      let schema = [aesonQQ| { type: "number" } |]
      assertValid schema [aesonQQ| 3 |]
      assertValid schema [aesonQQ| 3.5 |]
      assertInvalid schema [aesonQQ| true |]
      assertInvalid schema [aesonQQ| "nobody expects the ..." |]
      assertInvalid schema [aesonQQ| { eins: 1, zwei: 2 } |]
      assertInvalid schema [aesonQQ| ["eins", "zwei"] |]
      assertInvalid schema [aesonQQ| null |]
  , testCase "type: \"integer\"" $ do
      let schema = [aesonQQ| { type: "integer" } |]
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