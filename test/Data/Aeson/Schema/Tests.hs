module Data.Aeson.Schema.Tests
  ( tests
  ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HU

import Data.Foldable (toList)
import Data.Aeson
import Data.Text ()
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H

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
        Just val -> case fromJSON val :: Result (Schema V3 String) of
          Error e -> HU.assertFailure e
          Success schema -> do
            Just "http://json-schema.org/schema#" HU.@=? schemaId schema
            0 HU.@=? schemaMinItems schema
  , testCase "Foldable instance" $ do
      let schemaWithRef ref = empty { schemaDRef = Just ref }
      let schema = empty
            { schemaType = [Choice2of2 $ schemaWithRef "a"]
            , schemaProperties = H.fromList [("aProperty", schemaWithRef "b")]
            , schemaPatternProperties = let Right p = mkPattern "lorem.+" in [(p, schemaWithRef "c")]
            , schemaAdditionalProperties = Choice2of2 $ schemaWithRef "d"
            , schemaItems = Just $ Choice2of2 [schemaWithRef "e", schemaWithRef "f"]
            , schemaAdditionalItems = Choice2of2 $ schemaWithRef "g"
            , schemaDependencies = H.fromList [("aProperty", Choice2of2 $ schemaWithRef "h")]
            , schemaDisallow = [Choice2of2 $ schemaWithRef "i"]
            , schemaExtends = [schemaWithRef "j"]
            , schemaDRef = Just "k"
            }
      map (:[]) ['a'..'k'] HU.@=? toList schema
  ]
