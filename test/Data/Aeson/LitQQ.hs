{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Data.Aeson.LitQQ
  ( aesonLitQQ
  ) where

import           Control.Applicative              ((*>), (<*))
import           Data.Aeson.Parser                (value')
import           Data.Attoparsec.ByteString.Char8 (skipSpace)
import           Data.Attoparsec.Lazy             (Result (..), parse)
import           Data.ByteString.Lazy.Char8       (pack)
import           Language.Haskell.TH              (ExpQ)
import           Language.Haskell.TH.Quote        (QuasiQuoter (..))
import           Language.Haskell.TH.Syntax       (lift)

import           Data.Aeson.Schema.CodeGen

aesonLitQQ :: QuasiQuoter
aesonLitQQ = QuasiQuoter { quoteExp = aesonLit }

aesonLit :: String -> ExpQ
aesonLit jsonStr = case parse (skipSpace *> value' <* skipSpace) (pack jsonStr) of
  Done _ json -> lift json
  _           -> fail "not a valid JSON value"
