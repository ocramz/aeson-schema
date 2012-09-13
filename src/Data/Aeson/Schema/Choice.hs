{-# LANGUAGE TemplateHaskell #-}

module Data.Aeson.Schema.Choice where

import           Data.Aeson.Schema.Choice.TH (generateChoice)

$(fmap concat (mapM generateChoice [2..20]))
