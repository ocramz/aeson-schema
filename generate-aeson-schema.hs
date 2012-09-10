{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Prelude hiding (readFile)
import Data.Version (showVersion)
import System.Console.CmdArgs
import Data.Aeson.Parser (json)
import Data.ByteString (readFile)
import Data.Attoparsec.ByteString (parse, Result, IResult (..))
import Data.Aeson.Schema.CodeGen (generateModule)
import Language.Haskell.TH.Syntax (runQ)
import Data.Aeson (fromJSON)
import qualified Data.Aeson as A
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import qualified Data.Map as M

import Paths_aeson_schema (version)
import Data.Aeson.Schema (Schema (..), V3)

data GenerateArgs = GenerateArgs
  { modName :: String
  , inputSchema :: FilePath
  } deriving (Data, Show, Typeable)

generateArgs :: GenerateArgs
generateArgs = GenerateArgs
  { modName = "Schema" &= name "module-name" &= opt "Schema" &= help "Name of the generated module"
  , inputSchema = "" &= typFile &= argPos 0
  } &= summary ("generate-aeson-schema-" ++ showVersion version)

main :: IO ()
main = do
  args <- cmdArgs generateArgs
  contents <- readFile $ inputSchema args
  value <- iResultM $ parse json contents
  case fromJSON value :: A.Result (Schema V3 Text) of
    A.Error str -> fail str
    A.Success schema -> do
      let m = M.fromList [(pack "A", schema)]
      (code, _) <- runQ $ generateModule (pack $ modName args) m
      TIO.putStrLn code
  where
    iResultM :: (Monad m) => Result a -> m a
    iResultM (Fail _ _ err) = fail err
    iResultM (Partial _) = fail "unexpected end of file"
    iResultM (Done _ r) = return r
