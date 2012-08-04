import Test.Framework
import qualified Test.HUnit as HU
import Test.Framework.Providers.HUnit

import qualified Data.Aeson.Schema.Tests
import qualified Data.Aeson.Schema.Choice.Tests

main = defaultMain
  [ testGroup "Data.Aeson.Schema" Data.Aeson.Schema.Tests.tests
  , testGroup "Data.Aeson.Schema.Choice" Data.Aeson.Schema.Choice.Tests.tests
  ]
