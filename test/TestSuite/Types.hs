module TestSuite.Types
  ( SchemaTest (..)
  , SchemaTestCase (..)
  , readSchemaTests
  ) where

import           Control.Applicative              ((*>), (<$>), (<*), (<*>))
import           Control.Monad                    (forM)
import           Data.Aeson
import           Data.Aeson.Schema
import           Data.Aeson.Types                 (parseEither)
import           Data.Attoparsec.ByteString.Char8 (skipSpace)
import           Data.Attoparsec.Lazy             (Result (..), parse)
import qualified Data.ByteString.Lazy             as LBS
import           Data.List                        (isSuffixOf)
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text, pack)
import           Paths_aeson_schema               (getDataFileName)
import           System.Directory                 (getDirectoryContents)
import           System.FilePath                  ((</>))

data SchemaTest = SchemaTest
  { schemaTestDescription :: Text
  , schemaTestSchema      :: Schema Text
  , schemaTestCases       :: [SchemaTestCase]
  } deriving (Eq, Show)

instance FromJSON SchemaTest where
  parseJSON (Object o) = SchemaTest <$> (fromMaybe "" <$> o .:? "description")
                                    <*> (o .: "schema")
                                    <*> (o .: "tests")
  parseJSON _ = fail "expected an object"

data SchemaTestCase = SchemaTestCase
  { schemaTestCaseDescription :: Text
  , schemaTestCaseData        :: Value
  , schemaTestCaseValid       :: Bool
  } deriving (Eq, Show)

instance FromJSON SchemaTestCase where
  parseJSON (Object o) = SchemaTestCase <$> (o .: "description")
                                        <*> (o .: "data")
                                        <*> (o .: "valid")
  parseJSON _ = fail "expected an object"

-- | Read tests collected by Julian Berman (https://github.com/Julian/JSON-Schema-Test-Suite)
readSchemaTests :: FilePath -> IO [SchemaTest]
readSchemaTests dir' = do
  dir <- getDataFileName dir'
  contents <- getDirectoryContents dir
  let jsonFiles = filter (".json" `isSuffixOf`) contents
  fmap concat $ forM jsonFiles $ \file -> do
    let fullPath = dir </> file
    jsonBS <- LBS.readFile fullPath
    case parse (skipSpace *> json <* skipSpace) jsonBS of
      Done _ value -> case parseEither parseJSON value of
        Left err -> fail $ "couldn't parse file '" ++ fullPath ++ "': " ++ err
        Right v  -> return $ map (prependFileName file) v
      _ -> fail $ "not a valid json file: " ++ fullPath
  where
    prependFileName fileName schemaTest = schemaTest
      { schemaTestDescription = pack fileName <> ": " <> schemaTestDescription schemaTest
      }
