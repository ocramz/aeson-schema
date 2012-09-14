{-# LANGUAGE TemplateHaskell #-}

-- | This module implements generalized sum types. In theory, you could use
-- a type representing a choice between different options by nesting the Either
-- type. In practice, however, pattern matching against such a type can quickly
-- become unwieldy. This module defines data types and functions for any number
-- of choices from 2 to 20. The naming schema is based on Data.Either.
-- For example:
-- 
-- @
-- data Choice3 a b c = Choice1of3 a | Choice2of3 b | Choice3of3 c deriving (...)
-- choice3 :: (a -> x) -> (b -> x) -> (c -> x) -> Choice3 a b c -> x
-- mapChoice3 :: (a1 -> a2) -> (b1 -> b2) -> (c1 -> c2) -> Choice a1 b1 c1 -> Choice a2 b2 c2
-- choice1of3s :: [Choice3 a b c] -> [a]
-- choice2of3s :: [Choice3 a b c] -> [b]
-- choice3of3s :: [Choice3 a b c] -> [c]
-- @
module Data.Aeson.Schema.Choice where

import           Data.Aeson.Schema.Choice.TH (generateChoice)
import           Language.Haskell.TH         (mkName)
import           Language.Haskell.TH.Lift    (deriveLiftMany)

$(fmap concat (mapM generateChoice [2..20]))

$(deriveLiftMany $ map (mkName . ("Choice" ++) . show) ([2..20] :: [Int]))