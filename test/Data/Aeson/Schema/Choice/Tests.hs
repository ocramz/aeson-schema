{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Data.Aeson.Schema.Choice.Tests
  ( tests
  ) where

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import qualified Test.HUnit                           as HU
import           Test.QuickCheck                      (Gen, arbitrary, vectorOf)

import           Data.Aeson
import           Data.Char                            (toUpper)
import           Data.List                            (nub)
import           Data.Vector                          (fromList)

import           Data.Aeson.Schema.Choice

choiceShow3 :: Choice3 String Int Bool -> String
choiceShow3 (Choice1of3 s) = s
choiceShow3 (Choice2of3 i) = show i
choiceShow3 (Choice3of3 b) = if b then "yes" else "no"

choiceEqual3 :: (Eq a, Eq b, Eq c) => Choice3 a b c -> Choice3 a b c -> Bool
choiceEqual3 = (==)

choiceOrd3 :: (Ord a, Ord b, Ord c) => Choice3 a b c -> Choice3 a b c -> Ordering
choiceOrd3 = compare

choiceShow7 :: (Show a, Show b, Show c, Show d) => Choice7 String Int Bool a b c d -> String
choiceShow7 (Choice1of7 s) = s
choiceShow7 (Choice2of7 i) = show i
choiceShow7 (Choice3of7 b) = if b then "yes" else "no"
choiceShow7 c = show c

tests :: [Test]
tests =
  [ testCase "parseJSON" $ do
      Success (Choice1of3 3 :: Choice3 Int String [Bool]) HU.@=? fromJSON (Number 3)
      Success (Choice2of3 "lorem" :: Choice3 Int String [Bool]) HU.@=? fromJSON (String "lorem")
      Success (Choice3of3 [True,False,True] :: Choice3 Int String [Bool]) HU.@=? fromJSON (Array . fromList . map Bool $ [True,False,True])
  , testCase "toJSON" $ do
      object [ "Left" .= String "a" ] HU.@=? toJSON (Choice1of3 (Left 'a') :: Choice3 (Either Char Bool) () (Maybe String))
      Array (fromList []) HU.@=? toJSON (Choice2of3 () :: Choice3 (Either Char Bool) () (Maybe String))
      Null HU.@=? toJSON (Choice3of3 Nothing :: Choice3 (Either Char Bool) () (Maybe String))
  , testProperty "arbitrary" $ do
      xs <- vectorOf 100 (arbitrary :: Gen (Choice3 () () ()))
      return $ length (nub xs) == 3
  , testCase "choice3" $ do
      let rnd = round :: Double -> Int
      (3 :: Int) HU.@=? choice3 length id rnd (Choice1of3 ("abc" :: String))
      (4 :: Int) HU.@=? choice3 length id rnd (Choice2of3 4)
      (5 :: Int) HU.@=? choice3 length id rnd (Choice3of3 (4.6 :: Double))
  , testCase "mapChoice3" $ do
      let plus1 = (+1) :: Int -> Int
      Choice1of3 "LOREM" HU.@=? mapChoice3 (map toUpper) plus1 not (Choice1of3 "lorem")
      Choice2of3 4 HU.@=? mapChoice3 (map toUpper) plus1 not (Choice2of3 (3 :: Int))
      Choice3of3 True HU.@=? mapChoice3 (map toUpper) plus1 not (Choice3of3 False)
  , testCase "choice2of3s" $ do
      ["eins" :: String, "zwei", "drei"] HU.@=? choice2of3s [Choice1of3 True, Choice2of3 "eins", Choice2of3 "zwei", Choice3of3 '?', Choice2of3 "drei"]
  ]
