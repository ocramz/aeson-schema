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
import qualified Data.Map as M

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
  ]