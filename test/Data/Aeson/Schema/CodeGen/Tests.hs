{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, ExistentialQuantification, RankNTypes, ScopedTypeVariables, ImpredicativeTypes, TupleSections #-}

module Data.Aeson.Schema.CodeGen.Tests
  ( tests
  ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding (Result (..))
import Test.QuickCheck.Property (morallyDubiousIOProperty, Result (..), succeeded, failed)
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HU

import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Control.Applicative (pure, (<$>))
import Data.Char (isAscii, isPrint)
import Control.Monad (liftM2, (>=>), forever)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Lazy as HM
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (isNothing, listToMaybe)
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Aeson (Value (..))
import Data.Attoparsec.Number (Number (..))
import qualified Language.Haskell.Interpreter as Hint
import Language.Haskell.TH (runQ)
import Language.Haskell.TH.Ppr (pprint)
import qualified Language.Haskell.TH.Syntax as THS

import Data.Aeson.Schema
import Data.Aeson.Schema.Choice
import Data.Aeson.Schema.CodeGen (generateModule)
import Data.Aeson.Schema.Helpers (formatValidators, replaceHiddenModules, getUsedModules)

import Data.Aeson.Schema.Examples (examples)

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (HM.HashMap k v) where
  arbitrary = HM.fromList <$> arbitrary

instance Arbitrary Number where
  arbitrary = oneof
    [ I <$> arbitrary
    , D <$> arbitrary
    ]

instance (Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary

instance Arbitrary Pattern where
  -- TODO: improve performance
  arbitrary = do
    arbitraryString <- arbitrary
    case mkPattern arbitraryString of
      Just p -> return p
      Nothing -> arbitrary

instance Arbitrary Value where
  arbitrary = oneof
    [ Object <$> arbitrary
    , Array <$> arbitrary
    , String <$> arbitrary
    , Number <$> arbitrary
    , Bool <$> arbitrary
    , pure Null
    ]

instance Arbitrary SchemaType where
  arbitrary = oneof $ map pure [StringType, NumberType, IntegerType, BooleanType, ObjectType, ArrayType, NullType, AnyType]

--generateValidValue :: Schema V3 Text -> Gen Value
--generateValidValue schema = suchThat arbitrary (isNothing . validate schema)

arbitrarySchema :: (Eq a) => Int -> Gen (Schema V3 a)
arbitrarySchema 0 = return empty
arbitrarySchema depth = do
  typ <- shortListOf1 (choice2of arbitrary subSchema)
  required <- arbitrary
  disallow <- rareShortListOf (choice2of arbitrary subSchema)
  extends <- rareShortListOf subSchema
  let sch0 = empty
        { schemaType = typ
        , schemaRequired = required
        , schemaDisallow = disallow
        , schemaExtends = extends
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
        exclusiveMinimum <- if (isNothing sMinimum) then return False else arbitrary
        exclusiveMaximum <- if (isNothing sMaximum) then return False else arbitrary
        divisibleBy <- arbitrary
        return $ sch
          { schemaMinimum = sMinimum
          , schemaMaximum = sMaximum
          , schemaExclusiveMinimum = exclusiveMinimum
          , schemaExclusiveMaximum = exclusiveMaximum
          , schemaDivisibleBy = divisibleBy
          }

    ]
  let simple = sch1
  --enum <- maybeOf $ nub . take 3 <$> listOf1 generateValidValue (fmap undefined simple)
  enum <- return Nothing
  dflt <- case enum of
    Nothing -> return Nothing --maybeOf $ generateValidValue (fmap undefined simple)
    Just vs -> maybeOf $ oneof (map pure vs)
  return $ simple { schemaEnum = enum, schemaDefault = dflt }
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

instance (Eq a) => Arbitrary (Schema V3 a) where
  arbitrary = arbitrarySchema 3

data ForkLift = ForkLift (Chan (Hint.Interpreter (), Hint.InterpreterError -> IO ()))

-- | uses the Forklift pattern (http://apfelmus.nfshost.com/blog/2012/06/07-forklift.html)
--   to send commands to an interpreter running in a different thread
startInterpreterThread :: IO ForkLift
startInterpreterThread = do
  cmdChan <- newChan
  _ <- forkIO $ do
    errorHandler <- newEmptyMVar
    forever $ do
      Left err <- Hint.runInterpreter $ forever $ do
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

tests :: IO [Test]
tests = do
  forkLift <- startInterpreterThread
  return
    [ testProperty "generated code typechecks" $ typecheckGenerate forkLift
    , testGroup "examples" $ testExamples forkLift
    , testCase "1-tuple" $ do
        let
          schema = empty
            { schemaType = [Choice1of2 ArrayType]
            , schemaItems = Just $ Choice2of2 [empty { schemaType = [Choice1of2 NumberType] }]
            }
          graph = M.singleton "A" schema
        (code, _) <- runQ $ generateModule "TestOneTuple" graph
        result <- typecheck code forkLift
        case result of
          Left err -> HU.assertFailure $ show err
          Right _  -> return ()
    ]

typecheckGenerate :: ForkLift -> Schema V3 Text -> Property
typecheckGenerate forkLift schema = morallyDubiousIOProperty $ do
  let graph = M.singleton "A" schema
  (code, _) <- runQ $ generateModule "CustomSchema" graph
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
testExamples forkLift = examples testCase assertValid assertInvalid
  where
    assertValid = assertValidates True
    assertInvalid = assertValidates False
    assertValidates isValid graph schema value = do
      let graph' = if M.null graph then M.singleton "a" schema else graph
      (code, typeMap) <- runQ $ generateModule "TestSchema" graph'
      valueExpr <- replaceHiddenModules <$> runQ (THS.lift value)
      let typ = replaceHiddenModules $ typeMap M.! "a"
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
        Left err -> printInfo >> (HU.assertFailure $ show err)
        Right validates -> case (isValid, validates) of
          (True, Nothing) -> return ()
          (False, Just _) -> return ()
          (True, Just e)  -> do
            printInfo
            HU.assertFailure $ "value should have beean parsed but was rejected with error '" ++ e ++ "'"
          (False, Nothing) -> do
            printInfo
            HU.assertFailure "value should have been rejected"
