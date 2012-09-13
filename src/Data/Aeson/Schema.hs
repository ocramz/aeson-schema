{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

module Data.Aeson.Schema
  ( SchemaType (..)
  , Schema (..)
  , V3
  , Graph
  , Pattern (..)
  , mkPattern
  , empty
  ) where

import           Control.Applicative      ((<*>))
import           Control.Arrow            (second)
import           Control.Monad            (liftM)
import           Data.Aeson               (FromJSON (..), Value (..), (.!=),
                                           (.:?))
import           Data.Aeson.Types         (Parser, emptyArray, emptyObject)
import           Data.Attoparsec.Number   (Number (..))
import           Data.Foldable            (Foldable (..), toList)
import           Data.Function            (on)
import           Data.Functor             ((<$>))
import qualified Data.HashMap.Strict      as H
import qualified Data.Map                 as M
import           Data.Text                (Text, unpack)
import           Data.Traversable         (traverse)
import qualified Data.Vector              as V
import           Prelude                  hiding (foldr, length)
import           Text.Regex.PCRE          (makeRegexM)
import           Text.Regex.PCRE.String   (Regex)

import           Data.Aeson.Schema.Choice

type Map a = H.HashMap Text a

data Pattern = Pattern { patternSource :: Text, patternCompiled :: Regex }

instance Eq Pattern where
  (==) = (==) `on` patternSource

instance Show Pattern where
  show pattern = "let Right p = mkPattern (" ++ show (patternSource pattern) ++ ") in p"

instance FromJSON Pattern where
  parseJSON (String s) = mkPattern s
  parseJSON _ = fail "only strings can be parsed as patterns"

mkPattern :: (Monad m) => Text -> m Pattern
mkPattern t = liftM (Pattern t) $ makeRegexM (unpack t)

data SchemaType = StringType
                | NumberType
                | IntegerType
                | BooleanType
                | ObjectType
                | ArrayType
                | NullType
                | AnyType
                deriving (Eq, Show, Read)

instance FromJSON SchemaType where
  parseJSON (String t) = case t of
    "string"  -> return StringType
    "number"  -> return NumberType
    "integer" -> return IntegerType
    "boolean" -> return BooleanType
    "object"  -> return ObjectType
    "array"   -> return ArrayType
    "null"    -> return NullType
    "any"     -> return AnyType
    _         -> fail $ "not a valid type: " ++ unpack t
  parseJSON _ = fail "not a string"

data Schema version ref = Schema
  { schemaType :: [Choice2 SchemaType (Schema version ref)]
  , schemaProperties :: Map (Schema version ref)
  , schemaPatternProperties :: [(Pattern, Schema version ref)]
  , schemaAdditionalProperties :: Choice2 Bool (Schema version ref)
  , schemaItems :: Maybe (Choice2 (Schema version ref) [Schema version ref])
  , schemaAdditionalItems :: Choice2 Bool (Schema version ref)
  , schemaRequired :: Bool
  , schemaDependencies :: Map (Choice2 [Text] (Schema version ref))
  , schemaMinimum :: Maybe Number
  , schemaMaximum :: Maybe Number
  , schemaExclusiveMinimum :: Bool
  , schemaExclusiveMaximum :: Bool
  , schemaMinItems :: Int
  , schemaMaxItems :: Maybe Int
  , schemaUniqueItems :: Bool
  , schemaPattern :: Maybe Pattern
  , schemaMinLength :: Int
  , schemaMaxLength :: Maybe Int
  , schemaEnum :: Maybe [Value]
  , schemaEnumDescriptions :: Maybe [Text]
  , schemaDefault :: Maybe Value
  , schemaTitle :: Maybe Text
  , schemaDescription :: Maybe Text
  , schemaFormat :: Maybe Text
  , schemaDivisibleBy :: Maybe Number
  , schemaDisallow :: [Choice2 SchemaType (Schema version ref)]
  , schemaExtends :: [Schema version ref]
  , schemaId :: Maybe Text
  , schemaDRef :: Maybe ref -- ^ $ref
  , schemaDSchema :: Maybe Text -- ^ $schema
  } deriving (Eq, Show)

data V3

type Graph f ref = M.Map ref (f ref)

instance Functor (Schema version) where
  fmap f s = s
    { schemaType = mapChoice2 id (fmap f) <$> schemaType s
    , schemaProperties = fmap f <$> schemaProperties s
    , schemaPatternProperties = second (fmap f) <$> schemaPatternProperties s
    , schemaAdditionalProperties = mapChoice2 id (fmap f) (schemaAdditionalProperties s)
    , schemaItems = mapChoice2 (fmap f) (fmap $ fmap f) <$> schemaItems s
    , schemaAdditionalItems = mapChoice2 id (fmap f) (schemaAdditionalItems s)
    , schemaDependencies = mapChoice2 id (fmap f) <$> schemaDependencies s
    , schemaDisallow = mapChoice2 id (fmap f) <$> schemaDisallow s
    , schemaExtends = fmap f <$> schemaExtends s
    , schemaDRef = f <$> schemaDRef s
    }

instance Foldable (Schema version) where
  foldr f start s = ffoldr (ffoldr f) (choice2of2s $ schemaType s)
                  . ffoldr (ffoldr f) (schemaProperties s)
                  . ffoldr (ffoldr f) (map snd $ schemaPatternProperties s)
                  . foldChoice2of2 (ffoldr f) (schemaAdditionalProperties s)
                  . ffoldr (\items -> foldChoice1of2 (ffoldr f) items . foldChoice2of2 (ffoldr $ ffoldr f) items) (schemaItems s)
                  . foldChoice2of2 (ffoldr f) (schemaAdditionalItems s)
                  . ffoldr (ffoldr f) (choice2of2s $ toList $ schemaDependencies s)
                  . ffoldr (ffoldr f) (choice2of2s $ schemaDisallow s)
                  . ffoldr (ffoldr f) (schemaExtends s)
                  . ffoldr f (schemaDRef s)
                  $ start
    where
      ffoldr :: (Foldable t) => (a -> b -> b) -> t a -> b -> b
      ffoldr g = flip $ foldr g
      foldChoice1of2 :: (a -> b -> b) -> Choice2 a x -> b -> b
      foldChoice1of2 g (Choice1of2 c) = g c
      foldChoice1of2 _ _ = id
      foldChoice2of2 :: (a -> b -> b) -> Choice2 x a -> b -> b
      foldChoice2of2 g (Choice2of2 c) = g c
      foldChoice2of2 _ _ = id

empty :: Schema version ref
empty = Schema
  { schemaType = [Choice1of2 AnyType]
  , schemaProperties = H.empty
  , schemaPatternProperties = []
  , schemaAdditionalProperties = Choice1of2 True
  , schemaItems = Nothing
  , schemaAdditionalItems = Choice1of2 True
  , schemaRequired = False
  , schemaDependencies = H.empty
  , schemaMinimum = Nothing
  , schemaMaximum = Nothing
  , schemaExclusiveMinimum = False
  , schemaExclusiveMaximum = False
  , schemaMinItems = 0
  , schemaMaxItems = Nothing
  , schemaUniqueItems = False
  , schemaPattern = Nothing
  , schemaMinLength = 0
  , schemaMaxLength = Nothing
  , schemaEnum = Nothing
  , schemaEnumDescriptions = Nothing
  , schemaDefault = Nothing
  , schemaTitle = Nothing
  , schemaDescription = Nothing
  , schemaFormat = Nothing
  , schemaDivisibleBy = Nothing
  , schemaDisallow = []
  , schemaExtends = []
  , schemaId = Nothing
  , schemaDRef = Nothing
  , schemaDSchema = Nothing
  }

instance (FromJSON ref) => FromJSON (Schema V3 ref) where
  parseJSON (Object o) =
    Schema <$> (parseSingleOrArray =<< parseFieldDefault "type" "any")
           <*> parseFieldDefault "properties" emptyObject
           <*> (parseFieldDefault "patternProperties" emptyObject >>= mapM (\(k, v) -> fmap (,v) (mkPattern k)) . H.toList)
           <*> (parseField "additionalProperties" .!= Choice1of2 True)
           <*> parseField "items"
           <*> (parseField "additionalItems" .!= Choice1of2 True)
           <*> parseFieldDefault "required" (Bool False)
           <*> (traverse parseDependency =<< parseFieldDefault "dependencies" emptyObject)
           <*> parseField "minimum"
           <*> parseField "maximum"
           <*> parseFieldDefault "exclusiveMinimum" (Bool False)
           <*> parseFieldDefault "exclusiveMaximum" (Bool False)
           <*> parseFieldDefault "minItems" (Number $ fromInteger 0)
           <*> parseField "maxItems"
           <*> parseFieldDefault "uniqueItems" (Bool False)
           <*> parseField "pattern"
           <*> parseFieldDefault "minLength" (Number $ fromInteger 0)
           <*> parseField "maxLength"
           <*> parseField "enum"
           <*> parseField "enumDescriptions"
           <*> parseField "default"
           <*> parseField "title"
           <*> parseField "description"
           <*> parseField "format"
           <*> parseField "divisibleBy"
           <*> (parseSingleOrArray =<< parseFieldDefault "disallow" emptyArray)
           <*> ((maybe (return Nothing) (fmap Just . parseSingleOrArray) =<< parseField "extends") .!= [])
           <*> parseField "id"
           <*> parseField "$ref"
           <*> parseField "$schema"
      where
        parseField :: (FromJSON a) => Text -> Parser (Maybe a)
        parseField name = o .:? name
        parseFieldDefault :: (FromJSON a) => Text -> Value -> Parser a
        parseFieldDefault name value = parseJSON =<< parseField name .!= value

        parseDependency (String s) = return $ Choice1of2 [s]
        parseDependency val = parseJSON val
  parseJSON _ = fail "a schema must be a JSON object"

singleOrArray :: (Value -> Parser a) -> Value -> Parser [a]
singleOrArray p (Array a) = mapM p (V.toList a)
singleOrArray p v = (:[]) <$> p v

parseSingleOrArray :: (FromJSON a) => Value -> Parser [a]
parseSingleOrArray = singleOrArray parseJSON
