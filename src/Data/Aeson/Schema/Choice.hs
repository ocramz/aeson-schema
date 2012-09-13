{-# LANGUAGE TemplateHaskell #-}

module Data.Aeson.Schema.Choice where

import           Data.Aeson.Schema.Choice.TH (generateChoice)
import           Language.Haskell.TH         (mkName)
import           Language.Haskell.TH.Lift    (deriveLiftMany)

$(fmap concat (mapM generateChoice [2..20]))

$(deriveLiftMany $ map (mkName . ("Choice" ++) . show) ([2..20] :: [Int]))