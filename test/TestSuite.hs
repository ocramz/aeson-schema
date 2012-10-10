import           Control.Applicative               ((<$>))
import           Test.Framework

import qualified Data.Aeson.Schema.Choice.Tests
import qualified Data.Aeson.Schema.CodeGen.Tests
import qualified Data.Aeson.Schema.Types.Tests
import qualified Data.Aeson.Schema.Validator.Tests
import           TestSuite.Types                   (readSchemaTests)

main :: IO ()
main = readSchemaTests "test/test-suite/tests/draft3" >>= \schemaTests -> defaultMain
  [ testGroup "Data.Aeson.Schema.Types" Data.Aeson.Schema.Types.Tests.tests
  , testGroup "Data.Aeson.Schema.Validator" $ Data.Aeson.Schema.Validator.Tests.tests schemaTests
  , buildTest $ testGroup "Data.Aeson.Schema.CodeGen" <$> Data.Aeson.Schema.CodeGen.Tests.tests schemaTests
  , testGroup "Data.Aeson.Schema.Choice" Data.Aeson.Schema.Choice.Tests.tests
  ]
