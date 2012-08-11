{-# LANGUAGE FlexibleInstances, TupleSections #-}

module Data.Aeson.Schema
  ( Schema (..)
  , Pattern (..)
  , mkPattern
  , empty
  , Fix (..)
  , followReferences
  ) where

import Prelude hiding (foldr, length)
import Data.Maybe (fromMaybe, maybe, isNothing)
import Data.Foldable (Foldable (..), toList)
import Data.Traversable (traverse)
import qualified Data.List as L
import Data.Function (fix, on)
import Data.Functor ((<$>))
import Data.Ratio
import Control.Applicative ((<*>))
import Control.Arrow (second)
import Control.Monad ((=<<), mapM, forM_, sequence_, msum, liftM, when, void, MonadPlus (..), msum)
import Data.Aeson (Value (..), (.:?), (.!=), FromJSON (..))
import Data.Aeson.Types (Parser (..), emptyObject, emptyArray)
import qualified Data.Aeson as A
import Data.Aeson.Types (parse)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import Data.Text (Text (..), unpack, length)
import Data.Attoparsec.Number (Number (..))
import Text.Regex.PCRE (makeRegexM, match)
import Text.Regex.PCRE.String (Regex)

import Data.Aeson.Schema.Choice

type Map a = H.HashMap Text a

data Pattern = Pattern { patternSource :: Text, patternCompiled :: Regex }

instance Eq Pattern where
  (==) = (==) `on` patternSource

instance Show Pattern where
  show pattern = "let Right p = mkPattern (" ++ show (patternSource pattern) ++ ") in p"

instance FromJSON Pattern where
  parseJSON (String s) = mkPattern s

mkPattern :: (Monad m) => Text -> m Pattern
mkPattern t = liftM (Pattern t) $ makeRegexM (unpack t)

data Schema ref = Schema
  { schemaType :: [Choice2 Text (Schema ref)]
  , schemaProperties :: Map (Schema ref)
  , schemaPatternProperties :: [(Pattern, Schema ref)]
  , schemaAdditionalProperties :: Choice3 Text Bool (Schema ref)
  , schemaItems :: Maybe (Choice3 Text (Schema ref) [Schema ref])
  , schemaAdditionalItems :: Choice3 Text Bool (Schema ref)
  , schemaRequired :: Bool
  , schemaDependencies :: Map (Choice2 [Text] (Schema ref))
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
  , schemaDisallow :: [Choice2 Text (Schema ref)]
  , schemaExtends :: [Schema ref]
  , schemaId :: Maybe Text
  , schemaDRef :: Maybe ref -- ^ $ref
  , schemaDSchema :: Maybe Text -- ^ $schema
  } deriving (Eq, Show)

instance Functor Schema where
  fmap f s = s
    { schemaType = choice2 id (fmap f) <$> schemaType s
    , schemaProperties = fmap f <$> schemaProperties s
    , schemaPatternProperties = second (fmap f) <$> schemaPatternProperties s
    , schemaAdditionalProperties = choice3 id id (fmap f) (schemaAdditionalProperties s)
    , schemaItems = choice3 id (fmap f) (fmap $ fmap f) <$> schemaItems s
    , schemaAdditionalItems = choice3 id id (fmap f) (schemaAdditionalItems s)
    , schemaDependencies = choice2 id (fmap f) <$> schemaDependencies s
    , schemaDisallow = choice2 id (fmap f) <$> schemaDisallow s
    , schemaExtends = fmap f <$> schemaExtends s
    , schemaDRef = f <$> schemaDRef s
    }

instance Foldable Schema where
  foldr f start s = ffoldr (ffoldr f) (choice2of2s $ schemaType s)
                  . ffoldr (ffoldr f) (schemaProperties s)
                  . ffoldr (ffoldr f) (map snd $ schemaPatternProperties s)
                  . foldChoice3of3 (ffoldr f) (schemaAdditionalProperties s)
                  . ffoldr (\items -> foldChoice2of3 (ffoldr f) items . foldChoice3of3 (ffoldr $ ffoldr f) items) (schemaItems s)
                  . foldChoice3of3 (ffoldr f) (schemaAdditionalItems s)
                  . ffoldr (ffoldr f) (choice2of2s $ toList $ schemaDependencies s)
                  . ffoldr (ffoldr f) (choice2of2s $ schemaDisallow s)
                  . ffoldr (ffoldr f) (schemaExtends s)
                  . ffoldr f (schemaDRef s)
                  $ start
    where
      ffoldr :: (Foldable t) => (a -> b -> b) -> t a -> b -> b
      ffoldr g = flip $ foldr g
      foldChoice2of3 :: (a -> b -> b) -> Choice3 x a y -> b -> b
      foldChoice2of3 g (Choice2of3 c) = g c
      foldChoice2of3 _ _ = id
      foldChoice3of3 :: (a -> b -> b) -> Choice3 x y a -> b -> b
      foldChoice3of3 g (Choice3of3 c) = g c
      foldChoice3of3 _ _ = id

empty :: Schema ref
empty = Schema
  { schemaType = []
  , schemaProperties = H.empty
  , schemaPatternProperties = []
  , schemaAdditionalProperties = Choice2of3 True
  , schemaItems = Nothing
  , schemaAdditionalItems = Choice2of3 True
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

newtype Fix a = Fix (a (Fix a))

instance (FromJSON ref) => FromJSON (Schema ref) where
  parseJSON (Object o) =
    Schema <$> (parseSingleOrArray =<< parseFieldDefault "type" "any")
           <*> parseFieldDefault "properties" emptyObject
           <*> (parseFieldDefault "patternProperties" emptyObject >>= mapM (\(k, v) -> fmap (,v) (mkPattern k)) . H.toList)
           <*> (parseField "additionalProperties" .!= Choice2of3 True)
           <*> parseField "items"
           <*> (parseField "additionalItems" .!= Choice2of3 True)
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
        parseDependency o = parseJSON o
  parseJSON _ = fail "a schema must be a JSON object"

singleOrArray :: (Value -> Parser a) -> Value -> Parser [a]
singleOrArray p (Array a) = mapM p (V.toList a)
singleOrArray p v = (:[]) <$> p v

parseSingleOrArray :: (FromJSON a) => Value -> Parser [a]
parseSingleOrArray = singleOrArray parseJSON

followReferences :: (Ord k, Functor f) => M.Map k (f k) -> M.Map k (f (Fix f))
followReferences input = fix $ \output -> fmap (Fix . (M.!) output) <$> input
