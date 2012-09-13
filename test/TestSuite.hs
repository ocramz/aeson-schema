import           Test.Framework

import qualified Data.Aeson.Schema.Choice.Tests
import qualified Data.Aeson.Schema.CodeGen.Tests
import qualified Data.Aeson.Schema.Types.Tests
import qualified Data.Aeson.Schema.Validator.Tests

main :: IO ()
main = defaultMain
  [ testGroup "Data.Aeson.Schema.Types" Data.Aeson.Schema.Types.Tests.tests
  , testGroup "Data.Aeson.Schema.Validator" Data.Aeson.Schema.Validator.Tests.tests
  , buildTest $ fmap (testGroup "Data.Aeson.Schema.CodeGen") Data.Aeson.Schema.CodeGen.Tests.tests
  , testGroup "Data.Aeson.Schema.Choice" Data.Aeson.Schema.Choice.Tests.tests
  ]
