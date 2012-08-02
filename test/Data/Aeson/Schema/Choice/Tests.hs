{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Data.Aeson.Schema.Choice.Tests
  ( tests
  ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HU

import Data.Text ()
import Data.Vector (fromList)
import Data.Aeson
import Data.Aeson.Schema.Choice (generateChoice)
import Language.Haskell.TH

$(generateChoice 3)
$(generateChoice 7)

choiceShow3 :: Choice3 String Int Bool -> String
choiceShow3 (Choice1of3 s) = s
choiceShow3 (Choice2of3 i) = show i
choiceShow3 (Choice3of3 b) = if b then "yes" else "no"

choiceEqual3 :: (Eq a, Eq b, Eq c) => Choice3 a b c -> Choice3 a b c -> Bool
choiceEqual3 = (==)

choiceShow7 :: (Show a, Show b, Show c, Show d) => Choice7 String Int Bool a b c d -> String
choiceShow7 (Choice1of7 s) = s
choiceShow7 (Choice2of7 i) = show i
choiceShow7 (Choice3of7 b) = if b then "yes" else "no"
choiceShow7 c = show c

tests :: [Test]
tests =
  [ testCase "parseJSON" $ do
      Success (Choice1of3 3 :: Choice3 Int String [Bool]) HU.@=? fromJSON (Number $ fromInteger 3)
      Success (Choice2of3 "lorem" :: Choice3 Int String [Bool]) HU.@=? fromJSON (String "lorem")
      Success (Choice3of3 [True,False,True] :: Choice3 Int String [Bool]) HU.@=? fromJSON (Array . fromList . map Bool $ [True,False,True])
  , testCase "toJSON" $ do
      object [ "Left" .= String "a" ] HU.@=? toJSON (Choice1of3 (Left 'a') :: Choice3 (Either Char Bool) () (Maybe String))
      Array (fromList []) HU.@=? toJSON (Choice2of3 () :: Choice3 (Either Char Bool) () (Maybe String))
      Null HU.@=? toJSON (Choice3of3 Nothing :: Choice3 (Either Char Bool) () (Maybe String))
  ]