module Data.Aeson.Schema.Validator.Tests
  ( tests
  ) where

import           Test.Framework
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit                     as HU

import           Data.Aeson                     (Value)
import           Data.Text                      (Text)

import           Data.Aeson.Schema.Types
import           Data.Aeson.Schema.Validator

import           Data.Aeson.Schema.Examples     (examples)

assertValid, assertInvalid :: Graph Schema Text
                           -> Schema Text
                           -> Value
                           -> HU.Assertion
assertValid graph schema value = case validate graph schema value of
  [] -> return ()
  es -> HU.assertFailure $ unlines es
assertInvalid graph schema value = case validate graph schema value of
  [] -> HU.assertFailure "expected a validation error"
  _  -> return ()

tests :: [Test]
tests = examples testCase assertValid assertInvalid
