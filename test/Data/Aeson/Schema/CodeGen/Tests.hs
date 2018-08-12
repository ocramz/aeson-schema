{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TupleSections        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Aeson.Schema.CodeGen.Tests
  ( tests
  ) where

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import qualified Test.HUnit                           as HU
import           Test.QuickCheck                      hiding (Result (..))
import           Test.QuickCheck.Property (Result (reason), failed, succeeded)

import           Control.Concurrent                   (forkIO)
import           Control.Concurrent.Chan              (Chan, newChan, readChan,
                                                       writeChan)
import           Control.Concurrent.MVar              (newEmptyMVar, putMVar,
                                                       takeMVar)
import           Control.Monad                        (forever, liftM2, (>=>))
import           Control.Monad.Trans                  (liftIO)
import           Data.Aeson                           (Value (..))
import           Data.Char                            (isAscii, isPrint)
import           Data.Monoid
import           Data.Hashable                        (Hashable)
import qualified Data.HashMap.Lazy                    as HM
import qualified Data.Map                             as M
import           Data.Maybe                           (isNothing, listToMaybe)
import           Data.Scientific                      (Scientific)
import           Data.Text                            (Text, pack, unpack)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as TIO
import qualified Data.Vector                          as V
import qualified Language.Haskell.Interpreter         as Hint
import qualified Language.Haskell.Interpreter.Unsafe  as Hint
import           Language.Haskell.TH                  (runQ)
import           Language.Haskell.TH.Ppr              (pprint)
import qualified Language.Haskell.TH.Syntax           as THS
import           System.IO                            (hClose)
import           System.IO.Temp                       (withSystemTempFile)

import           Data.Aeson.Schema
import           Data.Aeson.Schema.Choice
import           Data.Aeson.Schema.CodeGen            (generateModule)
import           Data.Aeson.Schema.CodeGenM           (Options(..), defaultOptions)
import           Data.Aeson.Schema.Helpers            (formatValidators,
                                                       getUsedModules,
                                                       replaceHiddenModules)

import           Data.Aeson.Schema.Examples           (examples)
import           TestSuite.Types                      (SchemaTest (..),
                                                       SchemaTestCase (..))
import qualified System.Directory                     as DIR
import qualified System.FilePath                      as FP

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (HM.HashMap k v) where
  arbitrary = HM.fromList <$> arbitrary

instance Arbitrary Scientific where
  arbitrary = fromRational <$> arbitrary

instance (Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary

instance Arbitrary Pattern where
  -- TODO: improve performance
  arbitrary = do
    arbitraryString <- arbitrary
    case mkPattern arbitraryString of
      Just p -> return p
      Nothing -> arbitrary

arbitraryValue :: Int -> Gen Value
arbitraryValue 0 = return Null
arbitraryValue i = oneof
  [ Object . HM.fromList <$> shortListOf ((,) <$> arbitrary <*> subValue)
  , Array  . V.fromList  <$> shortListOf subValue
  , String <$> arbitrary
  , Number <$> arbitrary
  , Bool   <$> arbitrary
  , pure Null
  ]
  where
    subValue = arbitraryValue (i-1)
    shortListOf = fmap (take 3) . listOf


instance Arbitrary Value where
  arbitrary = arbitraryValue 3

instance Arbitrary SchemaType where
  arbitrary = elements [minBound .. maxBound]

arbitrarySchema :: Eq a => Int -> Gen (Schema a)
arbitrarySchema 0 = return empty
arbitrarySchema depth = do
  typ <- shortListOf1 (choice2of arbitrary subSchema)
  required <- arbitrary
  disallow <- rareShortListOf (choice2of arbitrary subSchema)
  extends <- rareShortListOf subSchema
  description <- arbitrary
  let sch0 = empty
        { schemaType = typ
        , schemaRequired = required
        , schemaDisallow = disallow
        , schemaExtends = extends
        , schemaDescription = description
        }
  sch1 <- flip (foldl (>=>) return) sch0
    [ modifyIf (Choice1of2 ArrayType `elem` typ) $ \sch -> do
        items <- maybeOf (choice2of subSchema $ shortListOf1 subSchema)
        additionalItems <- choice2of arbitrary subSchema
        minItems <- abs <$> arbitrary
        maxItems <- fmap ((+ minItems) . abs) <$> arbitrary
        uniqueItems <- arbitrary
        return $ sch
          { schemaItems = items
          , schemaAdditionalItems = additionalItems
          , schemaMinItems = minItems
          , schemaMaxItems = maxItems
          , schemaUniqueItems = uniqueItems
          }
    , modifyIf (Choice1of2 ObjectType `elem` typ) $ \sch -> do
        properties <- smallMapOf (T.filter (\c -> isPrint c && isAscii c) <$> arbitrary) subSchema
        patternProperties <- shortListOf (tupleOf arbitrary subSchema)
        additionalProperties <- choice2of arbitrary subSchema
        dependencies <- smallMapOf arbitrary (choice2of arbitrary subSchema)
        return $ sch
          { schemaProperties = properties
          , schemaPatternProperties = patternProperties
          , schemaAdditionalProperties = additionalProperties
          , schemaDependencies = dependencies
          }
    , modifyIf (Choice1of2 StringType `elem` typ) $ \sch -> do
        minLength <- abs <$> arbitrary
        maxLength <- fmap ((+ minLength) . abs) <$> arbitrary
        pattern <- arbitrary
        format <- maybeOf (oneof formats)
        return $ sch
          { schemaMinLength = minLength
          , schemaMaxLength = maxLength
          , schemaPattern = pattern
          , schemaFormat = format
          }
    , modifyIf (Choice1of2 NumberType `elem` typ || Choice1of2 IntegerType `elem` typ) $ \sch -> do
        sMinimum <- arbitrary
        sMaximum <- case sMinimum of
          Nothing -> arbitrary
          Just mini -> fmap ((+ mini) . abs) <$> arbitrary
        exclusiveMinimum <- if isNothing sMinimum then return False else arbitrary
        exclusiveMaximum <- if isNothing sMaximum then return False else arbitrary
        divisibleBy <- arbitrary
        return $ sch
          { schemaMinimum = sMinimum
          , schemaMaximum = sMaximum
          , schemaExclusiveMinimum = exclusiveMinimum
          , schemaExclusiveMaximum = exclusiveMaximum
          , schemaDivisibleBy = divisibleBy
          }

    ]
  -- the enum and default values are probibly not compatible with the schema
  -- but that's irrelevant since we're only interested if the generated code
  -- typechecks
  enum <- maybeOf (shortListOf arbitrary)
  dflt <- maybeOf $ case enum of
    Just vs@(_:_) -> elements vs
    _             -> arbitrary
  return $ sch1 { schemaEnum = enum, schemaDefault = dflt }
  where
    modifyIf :: (Monad m) => Bool -> (a -> m a) -> a -> m a
    modifyIf False _ = return
    modifyIf True  a = a
    subSchema = arbitrarySchema (depth-1)
    formats = map (pure . fst) formatValidators
    maybeOf = fmap listToMaybe . listOf
    shortListOf = fmap (take 2) . listOf
    rareShortListOf a = frequency [(2, return []), (1, shortListOf a)]
    shortListOf1 = fmap (take 2) . listOf1
    choice2of a b = oneof [Choice1of2 <$> a, Choice2of2 <$> b]
    tupleOf = liftM2 (,)
    smallMapOf k v = HM.fromList <$> shortListOf (tupleOf k v)

instance (Eq a) => Arbitrary (Schema a) where
  arbitrary = arbitrarySchema 3

data ForkLift = ForkLift (Chan (Hint.Interpreter (), Hint.InterpreterError -> IO ()))

getSandboxPackageDB :: IO (Maybe FilePath)
getSandboxPackageDB = do
    cwd <- DIR.getCurrentDirectory
    contents <- readFile $ cwd <> [FP.pathSeparator] <> "cabal.sandbox.config"
    return $ findPackageDB contents
    where
        packagedb = "package-db: "
        maybePackageDB :: [FilePath] -> Maybe FilePath
        maybePackageDB list = case list of
                                   [path] -> Just $ T.unpack $ T.replace packagedb "" (T.pack path)
                                   _ -> Nothing
        findPackageDB contents = maybePackageDB $ filter (\l -> T.isPrefixOf packagedb (T.pack l)) (lines contents)

getInterpreterArgs :: IO [String]
getInterpreterArgs = do
    mdb <- getSandboxPackageDB
    return $ case mdb of
                Just path -> ["-no-user-package-db", "-package-db " <> path]
                Nothing -> []

-- | uses the Forklift pattern (http://apfelmus.nfshost.com/blog/2012/06/07-forklift.html)
-- to send commands to an interpreter running in a different thread
startInterpreterThread :: IO ForkLift
startInterpreterThread = do
  cmdChan <- newChan
  _ <- forkIO $ do
    errorHandler <- newEmptyMVar
    args <- getInterpreterArgs

    forever $ do
      Left err <- Hint.unsafeRunInterpreterWithArgs args $ do
        forever $ do
            (action, handler) <- liftIO $ readChan cmdChan
            liftIO $ putMVar errorHandler handler
            action
            liftIO $ takeMVar errorHandler
      handler <- takeMVar errorHandler
      handler err
  return $ ForkLift cmdChan

carry :: ForkLift -> Hint.Interpreter a -> IO (Either Hint.InterpreterError a)
carry (ForkLift cmdChan) action = do
  result <- newEmptyMVar
  let successHandler = action >>= liftIO . putMVar result . Right
      errorHandler   = putMVar result . Left
  writeChan cmdChan (successHandler, errorHandler)
  takeMVar result

tests :: [SchemaTest] -> IO [Test]
tests schemaTests = do
  forkLift <- startInterpreterThread
  return
    [ testProperty "generated code typechecks" $ typecheckGenerate forkLift
    , testGroup "examples" $ testExamples forkLift
    , testGroup "JSON-Schema-Test-Suite" $ map (execSchemaTest forkLift) schemaTests
    , testCase "1-tuple" $ do
        let
          schema = empty
            { schemaType = [Choice1of2 ArrayType]
            , schemaItems = Just $ Choice2of2 [empty { schemaType = [Choice1of2 NumberType] }]
            }
          graph = M.singleton "A" schema
        (code, _) <- runQ $ generateModule "TestOneTuple" graph defaultOptions
        result <- typecheck code forkLift
        case result of
          Left err -> HU.assertFailure $ show err
          Right _  -> return ()
    , testCase "simple map" $ do
        let
          schema = [schemaQQ| {
              "type": "object",
              "additionalProperties": { "type": "number" }
            } |]
          graph = M.singleton "A" schema
        (code, _) <- runQ $ generateModule "SimpleMap" graph defaultOptions
        result <- typecheck code forkLift
        case result of
          Left err -> HU.assertFailure $ show err
          Right _  -> return ()
    ]

typecheckGenerate :: ForkLift -> Schema Text -> Property
typecheckGenerate forkLift schema = ioProperty $ do
  let graph = M.singleton "A" schema
  (code, _) <- runQ $ generateModule "CustomSchema" graph defaultOptions
  eitherToResult <$> typecheck code forkLift

withCodeTempFile :: Text -> (FilePath -> IO a) -> IO a
withCodeTempFile code action = case maybeName of
  Nothing -> fail "couldn't find module name"
  Just name -> withSystemTempFile (unpack name ++ ".hs") $ \path handle -> do
    TIO.hPutStrLn handle code
    hClose handle
    action path
  where
    maybeName = findName (T.lines code)
    findName (l:ls) = case T.words l of
      ("module":n:_) -> Just n
      _ -> findName ls
    findName _ = Nothing

eitherToResult :: Show err => Either err a -> Result
eitherToResult (Left err) = failed { reason = show err }
eitherToResult (Right _)  = succeeded

typecheck :: Text -> ForkLift -> IO (Either Hint.InterpreterError ())
typecheck code forkLift = withCodeTempFile code $ \path ->
  carry forkLift $ Hint.loadModules [path]

testExamples :: ForkLift -> [Test]
testExamples forkLift = examples testCase (assertValid forkLift) (assertInvalid forkLift)

execSchemaTest :: ForkLift -> SchemaTest -> Test
execSchemaTest forkLift schemaTest = testGroup testName testCases
  where
    testName = unpack $ schemaTestDescription schemaTest
    testCases = map execSchemaTestCase $ schemaTestCases schemaTest
    schema = schemaTestSchema schemaTest
    execSchemaTestCase schemaTestCase = testCase name assertion
      where
        name = unpack $ schemaTestCaseDescription schemaTestCase
        shouldBeValid = schemaTestCaseValid schemaTestCase
        testData = schemaTestCaseData schemaTestCase
        assertion = assertValidates shouldBeValid forkLift M.empty schema testData

assertValid, assertInvalid :: ForkLift -> Graph Schema Text -> Schema Text -> Value -> HU.Assertion
assertValid = assertValidates True
assertInvalid = assertValidates False

assertValidates :: Bool -> ForkLift -> Graph Schema Text -> Schema Text -> Value -> HU.Assertion
assertValidates shouldBeValid forkLift graph schema value = do
  let graph' = if M.null graph then M.singleton "a" schema else graph
      repMap = _replaceModules defaultOptions
  (code, typeMap) <- runQ $ generateModule "TestSchema" graph' defaultOptions
  valueExpr <- flip replaceHiddenModules repMap <$> runQ (THS.lift value)
  let typ = flip replaceHiddenModules repMap $ typeMap M.! "a"
  let validatesExpr = unlines
        [ "case DAT.parseEither parseJSON (" ++ pprint valueExpr ++ ") :: Either String (" ++ pprint typ ++ ") of"
        , "  Prelude.Left e  -> Just e"
        , "  Prelude.Right _ -> Nothing"
        ]
  result <- withCodeTempFile code $ \path -> carry forkLift $ do
    Hint.loadModules [path]
    Hint.setImportsQ $ map (,Nothing) (getUsedModules (valueExpr, typ)) ++
      [ ("TestSchema", Nothing)
      , ("Prelude", Nothing)
      , ("Data.Aeson.Types", Just "DAT")
      , ("Data.Ratio", Nothing)
      ]
    Hint.interpret validatesExpr (Hint.as :: Maybe String)
  let printInfo = TIO.putStrLn code >> putStrLn validatesExpr
  case result of
    Left err -> printInfo >> HU.assertFailure (show err)
    Right maybeError -> case (shouldBeValid, maybeError) of
      (True, Nothing) -> return ()
      (False, Just _) -> return ()
      (True, Just e)  -> do
        printInfo
        HU.assertFailure $ "value should have beean parsed but was rejected with error '" ++ e ++ "'"
      (False, Nothing) -> do
        printInfo
        HU.assertFailure "value should have been rejected"
