module Data.Aeson.Schema.Validator.Tests
  ( tests
  ) where

import           Test.Framework
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit                     as HU

import           Data.Aeson                     (Value)
import qualified Data.Map                       as M
import           Data.Text                      (Text, unpack)

import           Data.Aeson.Schema.Types
import           Data.Aeson.Schema.Validator

import           Data.Aeson.Schema.Examples     (examples)
import           TestSuite.Types                (SchemaTest (..),
                                                 SchemaTestCase (..))

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

tests :: [SchemaTest] -> [Test]
tests schemaTests = examples testCase assertValid assertInvalid ++ map buildSchemaTest schemaTests
  where
    buildSchemaTest :: SchemaTest -> Test
    buildSchemaTest schemaTest = testGroup testName cases
      where
        testName = unpack $ schemaTestDescription schemaTest
        cases = map (buildSchemaTestCase $ schemaTestSchema schemaTest) $ schemaTestCases schemaTest
    buildSchemaTestCase :: Schema Text -> SchemaTestCase -> Test
    buildSchemaTestCase schema schemaTestCase = testCase testName assertion
      where
        testName = unpack $ schemaTestCaseDescription schemaTestCase
        testData = schemaTestCaseData schemaTestCase
        graph = M.empty
        assertion = if schemaTestCaseValid schemaTestCase
          then assertValid graph schema testData
          else assertInvalid graph schema testData

