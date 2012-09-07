module Data.Aeson.Schema.Validator.Tests
  ( tests
  ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HU

import Data.Aeson (Value)
import Data.Text (Text)

import Data.Aeson.Schema
import Data.Aeson.Schema.Validator

import Data.Aeson.Schema.Examples (examples)

assertValid, assertInvalid :: Graph (Schema V3) Text
                           -> Schema V3 Text
                           -> Value
                           -> HU.Assertion
assertValid graph schema value = case validate graph schema value of
  Just e -> HU.assertFailure e
  Nothing -> return ()
assertInvalid graph schema value = case validate graph schema value of
  Just _ -> return ()
  Nothing -> HU.assertFailure "expected a validation error"

tests :: [Test]
tests = examples testCase assertValid assertInvalid