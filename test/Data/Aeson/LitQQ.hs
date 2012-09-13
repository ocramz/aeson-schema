{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Data.Aeson.LitQQ
  ( aesonLitQQ
  ) where

import Data.Aeson.Parser (value')
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax (lift)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Data.ByteString.Lazy.Char8 (pack)
import Data.Attoparsec.Lazy (parse, Result (..))
import Data.Attoparsec.Char8 (skipSpace)
import Control.Applicative ((<*), (*>))

import Data.Aeson.Schema.CodeGen () -- Lift instances

aesonLitQQ :: QuasiQuoter
aesonLitQQ = QuasiQuoter
  { quoteExp = aesonLit
  }

aesonLit :: String -> ExpQ
aesonLit jsonStr = case parse (skipSpace *> value' <* skipSpace) (pack jsonStr) of
  Done _ json -> lift json
  _           -> fail "not a valid json value"
