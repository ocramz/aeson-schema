{-# LANGUAGE FlexibleInstances #-}

module Data.Aeson.Schema.CodeGen.Tests
  ( tests
  ) where

import Test.Framework
import Test.Framework.Options (TestOptions' (..))
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Property (morallyDubiousIOProperty)

import GHC
import GHC.Paths (libdir)
import DynFlags (defaultLogAction)
import System.IO (hPutStrLn, hClose)
import System.IO.Temp (withSystemTempFile)
import StringBuffer (stringToStringBuffer)
import Control.Applicative (pure, (<$>), (<*>))
import Data.Char (isAscii, isPrint)
import Control.Monad (liftM2, (>=>), forM_)
import Data.Aeson.Schema
import Data.Hashable (Hashable)
import qualified Data.HashMap.Lazy as HM
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (isNothing, listToMaybe)
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Aeson (Value (..))
import Data.Attoparsec.Number (Number (..))
import Language.Haskell.TH (Dec, runQ)
import Language.Haskell.TH.Ppr (pprint)

import Data.Aeson.Schema.Choice
import Data.Aeson.Schema.CodeGen (generate)
import Data.Aeson.Schema.Validator (validate)
import Data.Aeson.Schema.Helpers (formatValidators)

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
  let sch = empty
        { schemaType = typ
        , schemaRequired = required
        , schemaDisallow = disallow
        , schemaExtends = extends
        }
  sch' <- flip (foldl (>=>) return) sch
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
        properties <- smallMapOf (T.filter (isPrint .&&. isAscii) <$> arbitrary) subSchema
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
  let simple = sch'
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
    (.&&.) f g a = f a && g a

instance (Eq a) => Arbitrary (Schema V3 a) where
  arbitrary = arbitrarySchema 3

tests :: [Test]
tests =
  [ testProperty "generated code typechecks" typecheckGenerate
  ]

typecheckGenerate :: Schema V3 Text -> Property
typecheckGenerate schema = morallyDubiousIOProperty $ do
  let m = M.fromList [("A", fmap undefined schema)]
  code <- (Left "module CustomSchema where" :) <$> runQ (generate m)
  typecheck code
  return True

typecheck :: [Either Text Dec] -> IO ()
typecheck code = withSystemTempFile "CustomSchema.hs" $ \path handle -> do
  forM_ code $ either (TIO.hPutStrLn handle) (hPutStrLn handle . pprint)
  hClose handle
  defaultErrorHandler defaultLogAction $ runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    let modName = mkModuleName "CustomSchema"
    let target = Target 
          { targetId = TargetFile path Nothing
          , targetAllowObjCode = False 
          , targetContents = Nothing
          }
    setTargets [target]
    load LoadAllTargets
    modSummary <- getModSummary modName
    parsedModule <- parseModule modSummary
    _ <- typecheckModule parsedModule
    return ()