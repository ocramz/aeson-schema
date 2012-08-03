module Data.Aeson.Schema.Tests
  ( tests
  ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HU

import Data.Aeson
import Data.Aeson.Schema
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromJust)

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
  ]