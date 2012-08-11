import Test.Framework

import qualified Data.Aeson.Schema.Tests
import qualified Data.Aeson.Schema.Validator.Tests
import qualified Data.Aeson.Schema.Choice.Tests

main = defaultMain
  [ testGroup "Data.Aeson.Schema" Data.Aeson.Schema.Tests.tests
  , testGroup "Data.Aeson.Schema.Validator" Data.Aeson.Schema.Validator.Tests.tests
  , testGroup "Data.Aeson.Schema.Choice" Data.Aeson.Schema.Choice.Tests.tests
  ]
