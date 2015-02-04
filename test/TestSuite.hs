import           Control.Applicative               ((<$>))
import           Test.Framework
import qualified Data.Text                         as T

import qualified Data.Aeson.Schema.Choice.Tests
import qualified Data.Aeson.Schema.CodeGen.Tests
import qualified Data.Aeson.Schema.Types.Tests
import qualified Data.Aeson.Schema.Validator.Tests
import           TestSuite.Types                   (SchemaTest(..), readSchemaTests)

allTests :: IO [SchemaTest]
allTests = do
  requiredTests <- readSchemaTests "test/test-suite/tests/draft3"
  optionalTests <- readSchemaTests "test/test-suite/tests/draft3/optional"
  return $ requiredTests ++ optionalTests


main :: IO ()
main = do
  schemaTests <- allTests
  defaultMain
    [ testGroup "Data.Aeson.Schema.Types" Data.Aeson.Schema.Types.Tests.tests
    , testGroup "Data.Aeson.Schema.Validator" $ Data.Aeson.Schema.Validator.Tests.tests schemaTests
    --, buildTest $ testGroup "Data.Aeson.Schema.CodeGen" <$> Data.Aeson.Schema.CodeGen.Tests.tests schemaTests
    --, testGroup "Data.Aeson.Schema.Choice" Data.Aeson.Schema.Choice.Tests.tests
    ]

runTest :: String -> IO ()
runTest tc = do
    at <- allTests
    let schemaTests = findTest at 
    if length schemaTests == 0 
        then fail "test not found" 
        else
            defaultMain 
              [
                testGroup "Data.Aeson.Schema.Validator" $ Data.Aeson.Schema.Validator.Tests.tests schemaTests
                , buildTest $ testGroup "Data.Aeson.Schema.CodeGen" <$> Data.Aeson.Schema.CodeGen.Tests.tests schemaTests
              ]


  where
    name = T.pack tc
    findTest tests = filter (\t -> schemaTestDescription t == name) tests
