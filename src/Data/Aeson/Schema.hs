-- | Validate all positive numbers:
-- 
-- >>> :set -XQuasiQuotes
-- >>> :m +Data.Aeson Data.Aeson.Schema Data.Map Data.Text
-- >>> let positiveNumbers = [schemaQQ| { "type": "number", "minimum": 0, "exclusiveMinimum": true } |]
-- >>> validate Data.Map.empty positiveNumbers (Number $ fromInteger 1)
-- []
-- >>> validate Data.Map.empty positiveNumbers (Number $ fromInteger 0)
-- ["number must be greater than 0"]
-- >>> validate Data.Map.empty positiveNumbers (String $ pack "lorem")
-- ["type mismatch: expected NumberType but got string"]
module Data.Aeson.Schema
  ( module Data.Aeson.Schema.Types
  , module Data.Aeson.Schema.Validator
  ) where

import Data.Aeson.Schema.Types
import Data.Aeson.Schema.Validator