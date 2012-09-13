{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Data.Aeson.Schema.Types
  ( Pattern (..)
  , mkPattern
  , Map
  , SchemaType (..)
  , Schema (..)
  , Graph
  , empty
  , schemaQQ
  ) where

import           Control.Applicative        ((<*>))
import           Control.Applicative        ((*>), (<*))
import           Control.Arrow              (second)
import           Control.Monad              (liftM)
import           Data.Aeson                 (FromJSON (..), Value (..), (.!=),
                                             (.:?))
import           Data.Aeson.Parser          (value')
import           Data.Aeson.Schema.Choice
import           Data.Aeson.Types           (Parser, emptyArray, emptyObject,
                                             parseEither)
import           Data.Attoparsec.Char8      (skipSpace)
import           Data.Attoparsec.Lazy       (Result (..), parse)
import           Data.Attoparsec.Number     (Number (..))
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Foldable              (Foldable (..), toList)
import           Data.Function              (on)
import           Data.Functor               ((<$>))
import qualified Data.HashMap.Strict        as H
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (Text, unpack)
import           Data.Traversable           (traverse)
import qualified Data.Vector                as V
import           Language.Haskell.TH.Quote  (QuasiQuoter (..))
import           Language.Haskell.TH        (varE, recUpdE)
import           Language.Haskell.TH.Syntax (Lift (..))
import           Prelude                    hiding (foldr, length)
import           Text.Regex.PCRE            (makeRegexM)
import           Text.Regex.PCRE.String     (Regex)
import           Data.Aeson.TH.Lift         ()

-- | Compiled regex and its source
data Pattern = Pattern { patternSource :: Text, patternCompiled :: Regex }

instance Eq Pattern where
  (==) = (==) `on` patternSource

instance Show Pattern where
  show pattern = "let Right p = mkPattern (" ++ show (patternSource pattern) ++ ") in p"

instance FromJSON Pattern where
  parseJSON (String s) = mkPattern s
  parseJSON _ = fail "only strings can be parsed as patterns"

instance Lift Pattern where
  lift (Pattern src _) = [| let Right p = mkPattern src in p |]

-- | Compile a regex to a pattern, reporting errors with fail
mkPattern :: (Monad m) => Text -> m Pattern
mkPattern t = liftM (Pattern t) $ makeRegexM (unpack t)

-- | Primitive JSON types
data SchemaType = StringType
                | NumberType
                | IntegerType
                | BooleanType
                | ObjectType
                | ArrayType
                | NullType
                | AnyType -- ^ any of the above
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

instance Lift SchemaType where
  lift StringType  = [| StringType |]
  lift NumberType  = [| NumberType |]
  lift IntegerType = [| IntegerType |]
  lift BooleanType = [| BooleanType |]
  lift ObjectType  = [| ObjectType |]
  lift ArrayType   = [| ArrayType |]
  lift NullType    = [| NullType |]
  lift AnyType     = [| AnyType |]

type Map a = H.HashMap Text a

data Schema ref = Schema
  { schemaType                 :: [Choice2 SchemaType (Schema ref)]         -- ^ a list of allowed schema types
  , schemaProperties           :: Map (Schema ref)                          -- ^ subschemas for properties
  , schemaPatternProperties    :: [(Pattern, Schema ref)]                   -- ^ all properties that match one of the regexes must validate against the associated schema
  , schemaAdditionalProperties :: Choice2 Bool (Schema ref)                 -- ^ whether additional properties are allowed when the instance is an object, and if so, a schema that they have to validate against
  , schemaItems                :: Maybe (Choice2 (Schema ref) [Schema ref]) -- ^ either a schema for all array items or a different schema for each position in the array
  , schemaAdditionalItems      :: Choice2 Bool (Schema ref)                 -- ^ whether additional items are allowed
  , schemaRequired             :: Bool                                      -- ^ when this schema is used in a property of another schema, this means that the property must have a value and not be undefined
  , schemaDependencies         :: Map (Choice2 [Text] (Schema ref))         -- ^ map of dependencies (property a requires properties b and c, property a requires the instance to validate against another schema, etc.)
  , schemaMinimum              :: Maybe Number                              -- ^ minimum value when the instance is a number
  , schemaMaximum              :: Maybe Number                              -- ^ maximum value when the instance is a number
  , schemaExclusiveMinimum     :: Bool                                      -- ^ whether the minimum value is exclusive (only numbers greater than the minimum are allowed)
  , schemaExclusiveMaximum     :: Bool                                      -- ^ whether the maximum value is exclusive (only numbers less than the maximum are allowed)
  , schemaMinItems             :: Int                                       -- ^ minimum length for arrays
  , schemaMaxItems             :: Maybe Int                                 -- ^ maximum length for arrays
  , schemaUniqueItems          :: Bool                                      -- ^ whether all array items must be distinct from each other
  , schemaPattern              :: Maybe Pattern                             -- ^ regex for validating strings
  , schemaMinLength            :: Int                                       -- ^ minimum length for strings
  , schemaMaxLength            :: Maybe Int                                 -- ^ maximum length for strings
  , schemaEnum                 :: Maybe [Value]                             -- ^ allowed values for this schema
  , schemaEnumDescriptions     :: Maybe [Text]                              -- ^ extension by Google: description for the values in schemaEnum
  , schemaDefault              :: Maybe Value                               -- ^ default value if this schema is used in a property of another schema and the value is undefined
  , schemaTitle                :: Maybe Text                                -- ^ short description of the instance property
  , schemaDescription          :: Maybe Text                                -- ^ full description of the purpose of the instance property
  , schemaFormat               :: Maybe Text                                -- ^ format of strings, e.g. 'data-time', 'regex' or 'email'
  , schemaDivisibleBy          :: Maybe Number                              -- ^ when the instance is a number, it must be divisible by this number with no remainder
  , schemaDisallow             :: [Choice2 SchemaType (Schema ref)]         -- ^ list of disallowed types
  , schemaExtends              :: [Schema ref]                              -- ^ base schema that the current schema inherits from
  , schemaId                   :: Maybe Text                                -- ^ identifier of the current schema
  , schemaDRef                 :: Maybe ref                                 -- ^ $ref: reference to another schema
  , schemaDSchema              :: Maybe Text                                -- ^ $schema: URI of a schema that defines the format of the current schema
  } deriving (Eq, Show)

-- | Set of potentially mutually recursive schemas
type Graph f ref = M.Map ref (f ref)

instance Functor Schema where
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

instance Foldable Schema where
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

instance FromJSON ref => FromJSON (Schema ref) where
  parseJSON (Object o) = Schema
    <$> (parseSingleOrArray =<< parseFieldDefault "type" "any")
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

      singleOrArray :: (Value -> Parser a) -> Value -> Parser [a]
      singleOrArray p (Array a) = mapM p (V.toList a)
      singleOrArray p v = (:[]) <$> p v

      parseSingleOrArray :: (FromJSON a) => Value -> Parser [a]
      parseSingleOrArray = singleOrArray parseJSON

      parseDependency :: FromJSON ref => Value -> Parser (Choice2 [Text] (Schema ref))
      parseDependency (String s) = return $ Choice1of2 [s]
      parseDependency val = parseJSON val
  parseJSON _ = fail "a schema must be a JSON object"

instance (Eq ref, Lift ref) => Lift (Schema ref) where
  lift schema = case updates of
    [] -> varE 'empty
    _  -> recUpdE (varE 'empty) updates
    where
      updates = catMaybes
        [ field 'schemaType schemaType
        , field 'schemaProperties schemaProperties
        , field 'schemaPatternProperties schemaPatternProperties
        , field 'schemaAdditionalProperties schemaAdditionalProperties
        , field 'schemaItems schemaItems
        , field 'schemaAdditionalItems schemaAdditionalItems
        , field 'schemaRequired schemaRequired
        , field 'schemaDependencies schemaDependencies
        , field 'schemaMinimum schemaMinimum
        , field 'schemaMaximum schemaMaximum
        , field 'schemaExclusiveMinimum schemaExclusiveMinimum
        , field 'schemaExclusiveMaximum schemaExclusiveMaximum
        , field 'schemaMinItems schemaMinItems
        , field 'schemaMaxItems schemaMaxItems
        , field 'schemaUniqueItems schemaUniqueItems
        , field 'schemaPattern schemaPattern
        , field 'schemaMinLength schemaMinLength
        , field 'schemaMaxLength schemaMaxLength
        , field 'schemaEnum schemaEnum
        , field 'schemaEnumDescriptions schemaEnumDescriptions
        , field 'schemaDefault schemaDefault
        , field 'schemaTitle schemaTitle
        , field 'schemaDescription schemaDescription
        , field 'schemaFormat schemaFormat
        , field 'schemaDivisibleBy schemaDivisibleBy
        , field 'schemaDisallow schemaDisallow
        , field 'schemaExtends schemaExtends
        , field 'schemaId schemaId
        , fmap ('schemaDRef,) . lift . Just <$> schemaDRef schema
        , field 'schemaDSchema schemaDSchema
        ]
      field name accessor = if accessor schema == accessor empty
        then Nothing
        else Just $ (name,) <$> lift (accessor schema)

-- | The empty schema accepts any JSON value.
empty :: Schema ref
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

schemaQQ :: QuasiQuoter
schemaQQ = QuasiQuoter { quoteExp = quote }
  where
    quote jsonStr = case parse (skipSpace *> value' <* skipSpace) (pack jsonStr) of
      Done _ json -> case parseEither parseJSON json :: Either String (Schema Text) of
        Left e -> fail e
        Right s -> lift s
      _ -> fail "not a valid JSON value"
