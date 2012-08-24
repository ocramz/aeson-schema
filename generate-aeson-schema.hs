{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Prelude hiding (readFile)
import Control.Monad (forM_)
import Data.Version (showVersion)
import System.Console.CmdArgs
import Data.Aeson.Parser (json)
import Data.ByteString (readFile)
import Data.Attoparsec.ByteString (parse, Result, IResult (..))
import Data.Aeson.Schema.CodeGen (generate)
import Language.Haskell.TH.Syntax (runQ)
import Language.Haskell.TH.Ppr (pprint)
import Data.Aeson (fromJSON)
import qualified Data.Aeson as A
import Data.Text (Text, pack, unpack)
import qualified Data.Map as M

import Paths_aeson_schema (version)
import Data.Aeson.Schema (Schema (..), V3)

data GenerateArgs = GenerateArgs
  { inputSchema :: FilePath
  } deriving (Data, Show, Typeable)

generateArgs :: GenerateArgs
generateArgs = GenerateArgs
  { inputSchema = "" &= typFile &= argPos 0
  } &= summary ("generate-aeson-schema-" ++ showVersion version)

main :: IO ()
main = do
  args <- cmdArgs generateArgs
  contents <- readFile $ inputSchema args
  value <- iResultM $ parse json contents
  case fromJSON value :: A.Result (Schema V3 Text) of
    A.Error str -> fail str
    A.Success schema -> do
      let m = M.fromList [(pack "A", fmap undefined schema)]
      code <- runQ $ generate m
      forM_ code $ putStrLn . either unpack pprint
  where
    iResultM :: (Monad m) => Result a -> m a
    iResultM (Fail _ _ err) = fail err
    iResultM (Partial _) = fail "unexpected end of file"
    iResultM (Done _ r) = return r
