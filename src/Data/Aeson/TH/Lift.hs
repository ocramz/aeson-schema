{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Aeson.TH.Lift () where

import           Data.Aeson                 (Value (..))
import           Data.Attoparsec.Number     (Number (..))
import qualified Data.HashMap.Lazy          as HM
import           Data.Text                  (Text, pack, unpack)
import qualified Data.Vector                as V
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (Lift (..))

instance Lift Text where
  lift txt = [| pack $(lift (unpack txt)) |]

instance Lift Double where
  lift d = [| fromRational $(litE . rationalL . toRational $ d) :: Double |]

instance Lift Number where
  lift (I i) = [| I i |]
  lift (D d) = [| D d |]

instance (Lift k, Lift v) => Lift (HM.HashMap k v) where
  lift hm = [| HM.fromList $(lift (HM.toList hm)) |]

instance (Lift a) => Lift (V.Vector a) where
  lift vec = [| V.fromList $(lift (V.toList vec)) |]

instance Lift Value where
  lift (Object o) = [| Object o |]
  lift (Array a)  = [| Array a |]
  lift (String t) = [| String t |]
  lift (Number n) = [| Number n |]
  lift (Bool b)   = [| Bool b |]
  lift Null       = [| Null |]
