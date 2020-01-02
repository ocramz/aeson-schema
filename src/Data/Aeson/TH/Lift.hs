{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Data.Aeson.TH.Lift () where

import qualified Data.HashMap.Lazy          as HM
import qualified Data.Scientific            as S
import           Data.Text                  (Text, pack, unpack)
import qualified Data.Vector                as V
import           Language.Haskell.TH.Syntax (Lift (..))

#if ! MIN_VERSION_text(1,2,4)
instance Lift Text where
  lift txt = [| pack $(lift (unpack txt)) |]
#endif

#if ! MIN_VERSION_template_haskell(2,10,0)
instance Lift Double where
  lift d = [| fromRational $(litE . rationalL . toRational $ d) :: Double |]
#endif

instance (Lift k, Lift v) => Lift (HM.HashMap k v) where
  lift hm = [| HM.fromList $(lift (HM.toList hm)) |]

instance (Lift a) => Lift (V.Vector a) where
  lift vec = [| V.fromList $(lift (V.toList vec)) |]

instance Lift S.Scientific where
  lift s =
    let c = S.coefficient s
        e = S.base10Exponent s
    in [| S.scientific c e |]
