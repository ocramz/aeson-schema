{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Data.Aeson.Schema
  ( Schema (..)
  , Fix (..)
  , followReferences
  ) where

import Data.Maybe (fromMaybe, maybe)
import Data.Traversable (traverse)
import Data.List (concat)
import Data.Function (fix)
import Data.Functor ((<$>))
import Control.Monad ((=<<), mapM)
import Data.Aeson (Value (..), (.:?), (.!=), FromJSON (..))
import Data.Aeson.Types (Parser (..), emptyObject, emptyArray)
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import Data.Text (Text (..), unpack)
import Data.Attoparsec.Number (Number (..))

import Data.Aeson.Schema.Choice

type Map a = H.HashMap Text a

data Schema ref = Schema
  { schemaType :: [Choice2 String (Schema ref)]
  , schemaProperties :: Map (Schema ref)
  , schemaPatternProperties :: Map (Schema ref)
  , schemaAdditionalProperties :: Choice3 String Bool (Schema ref)
  , schemaItems :: Choice3 String (Schema ref) [Schema ref]
  , schemaAdditionalItems :: Choice3 String Bool (Schema ref)
  , schemaRequired :: Bool
  , schemaDependencies :: Map (Choice2 [String] (Schema ref))
  , schemaMinimum :: Maybe Number
  , schemaMaximum :: Maybe Number
  , schemaExclusiveMinimum :: Bool
  , schemaExclusiveMaximum :: Bool
  , schemaMinItems :: Int
  , schemaMaxItems :: Maybe Int
  , schemaUniqueItems :: Bool
  , schemaPattern :: Maybe String
  , schemaMinLength :: Int
  , schemaMaxLength :: Maybe Int
  , schemaEnum :: Maybe [Value]
  , schemaEnumDescriptions :: Maybe [String]
  , schemaDefault :: Maybe Value
  , schemaTitle :: Maybe String
  , schemaDescription :: Maybe String
  , schemaFormat :: Maybe String
  , schemaDivisibleBy :: Maybe Number
  , schemaDisallow :: [Choice2 String (Schema ref)]
  , schemaExtends :: [Schema ref]
  , schemaId :: Maybe String
  , schemaDRef :: Maybe ref -- ^ $ref
  , schemaDSchema :: Maybe String -- ^ $schema
  } deriving (Eq, Show)

instance Functor Schema where
  fmap f s = s
    { schemaType = choice2 id (fmap f) <$> schemaType s
    , schemaProperties = fmap f <$> schemaProperties s
    , schemaPatternProperties = fmap f <$> schemaPatternProperties s
    , schemaAdditionalProperties = choice3 id id (fmap f) (schemaAdditionalProperties s)
    , schemaItems = choice3 id (fmap f) (fmap $ fmap f) (schemaItems s)
    , schemaAdditionalItems = choice3 id id (fmap f) (schemaAdditionalItems s)
    , schemaDependencies = choice2 id (fmap f) <$> schemaDependencies s
    , schemaDisallow = choice2 id (fmap f) <$> schemaDisallow s
    , schemaExtends = fmap f <$> schemaExtends s
    , schemaDRef = f <$> schemaDRef s
    }

newtype Fix a = Fix (a (Fix a))

instance FromJSON (Schema String) where
  parseJSON (Object o) = do
    sType <- parseSingleOrArray =<< parseFieldDefault "type" "any"
    sProperties <- parseFieldDefault "properties" emptyObject
    sPatternProperties <- parseFieldDefault "patternProperties" emptyObject
    sAdditionalProperties <- parseField "additionalProperties" .!= Choice3of3 emptySchema
    sItems <- parseField "items" .!= Choice2of3 emptySchema
    sAdditionalItems <- parseField "additionalItems" .!= Choice3of3 emptySchema
    sRequired <- parseFieldDefault "required" (Bool False)
    sDependencies <- traverse parseDependency =<< parseFieldDefault "dependencies" emptyObject
    sMinimum <- parseField "minimum"
    sMaximum <- parseField "maximum"
    sExclusiveMinimum <- parseFieldDefault "exclusiveMinimum" (Bool False)
    sExclusiveMaximum <- parseFieldDefault "exclusiveMaximum" (Bool False)
    sMinItems <- parseFieldDefault "minItems" $ Number (fromInteger 0)
    sMaxItems <- parseField "maxItems"
    sUniqueItems <- parseFieldDefault "uniqueItems" (Bool False)
    sPattern <- parseField "pattern"
    sMinLength <- parseFieldDefault "minLength" $ Number (fromInteger 0)
    sMaxLength <- parseField "maxLength"
    sEnum <- parseField "enum"
    sEnumDescriptions <- parseField "enumDescriptions"
    sDefault <- parseField "default"
    sTitle <- parseField "title"
    sDescription <- parseField "description"
    sFormat <- parseField "format"
    sDivisibleBy <- parseField "divisibleBy"
    sDisallow <- parseSingleOrArray =<< parseFieldDefault "disallow" emptyArray
    sExtends <- (maybe (return Nothing) (fmap Just . parseSingleOrArray) =<< parseField "extends") .!= [emptySchema]
    sId <- parseField "id"
    sDRef <- parseField "$ref"
    sDSchema <- parseField "$schema"
    return $ Schema
      { schemaType = sType
      , schemaProperties = sProperties
      , schemaPatternProperties = sPatternProperties
      , schemaAdditionalProperties = sAdditionalProperties
      , schemaItems = sItems
      , schemaAdditionalItems = sAdditionalItems
      , schemaRequired = sRequired
      , schemaDependencies = sDependencies
      , schemaMinimum = sMinimum
      , schemaMaximum = sMaximum
      , schemaExclusiveMinimum = sExclusiveMinimum
      , schemaExclusiveMaximum = sExclusiveMaximum
      , schemaMinItems = sMinItems
      , schemaMaxItems = sMaxItems
      , schemaUniqueItems = sUniqueItems
      , schemaPattern = sPattern
      , schemaMinLength = sMinLength
      , schemaMaxLength = sMaxLength
      , schemaEnum = sEnum
      , schemaEnumDescriptions = sEnumDescriptions
      , schemaDefault = sDefault
      , schemaTitle = sTitle
      , schemaDescription = sDescription
      , schemaFormat = sFormat
      , schemaDivisibleBy = sDivisibleBy
      , schemaDisallow = sDisallow
      , schemaExtends = sExtends
      , schemaId = sId
      , schemaDRef = sDRef
      , schemaDSchema = sDSchema
      }
      where
        parseField :: (FromJSON a) => Text -> Parser (Maybe a)
        parseField name = o .:? name
        parseFieldDefault :: (FromJSON a) => Text -> Value -> Parser a
        parseFieldDefault name value = parseJSON =<< parseField name .!= value

        parseDependency (String s) = return $ Choice1of2 [unpack s]
        parseDependency o = parseJSON o
        emptySchema = case A.fromJSON emptyObject of
          A.Error e -> error e
          A.Success schema -> schema
  parseJSON _ = fail "a schema must be a JSON object"

singleOrArray :: (Value -> Parser a) -> Value -> Parser [a]
singleOrArray p (Array a) = mapM p (V.toList a)
singleOrArray p v = (:[]) <$> p v

parseSingleOrArray :: (FromJSON a) => Value -> Parser [a]
parseSingleOrArray = singleOrArray parseJSON

followReferences :: (Ord k, Functor f) => M.Map k (f k) -> M.Map k (f (Fix f))
followReferences input = fix $ \output -> fmap (Fix . (M.!) output) <$> input