{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections #-}

module Data.Aeson.Schema
  ( Schema (..)
  , Pattern (..)
  , mkPattern
  , empty
  , Fix (..)
  , followReferences
  , validate
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

validate :: Schema String -> Value -> Maybe String
validate schema val = case parse (validateP schema) val of
  A.Error e -> Just e
  A.Success _ -> Nothing

validateP :: Schema String -> Value -> Parser ()
validateP schema val = do
  msum $ map validateType (schemaType schema)
  let checkEnum e = assert (val `elem` e) "value has to be one of the values in enum"
  maybeCheck checkEnum $ schemaEnum schema
  forM_ (schemaDisallow schema) validateTypeDisallowed
  where
    validateType :: Choice2 Text (Schema String) -> Parser ()
    validateType (Choice1of2 t) = case (t, val) of
      ("string", String str) -> validateString schema str
      ("number", Number num) -> validateNumber schema num
      ("integer", Number (I _)) -> validateType (Choice1of2 "number")
      ("boolean", Bool _) -> return ()
      ("object", Object obj) -> validateObject schema obj
      ("array", Array arr) -> validateArray schema arr
      ("null", Null) -> return ()
      ("any", _) -> do
        msum $ map (validateType . Choice1of2) ["string", "number", "boolean", "object", "array", "null"]
      (typ, _) -> fail $ "type mismatch: expected " ++ unpack typ ++ " but got " ++ getType val
    validateType (Choice2of2 s) = validateP s val

    getType :: A.Value -> String
    getType (String _) = "string"
    getType (Number _) = "number"
    getType (Bool _)   = "boolean"
    getType (Object _) = "object"
    getType (Array _)  = "array"
    getType Null       = "null"

    validateTypeDisallowed :: Choice2 Text (Schema String) -> Parser ()
    validateTypeDisallowed (Choice1of2 t) = case (t, val) of
      ("string", String _) -> fail "strings are disallowed"
      ("number", Number _) -> fail "numbers are disallowed"
      ("integer", Number (I _)) -> fail "integers are disallowed"
      ("boolean", Bool _) -> fail "booleans are disallowed"
      ("object", Object _) -> fail "objects are disallowed"
      ("array", Array _) -> fail "arrays are disallowed"
      ("null", Null) -> fail "null is disallowed"
      ("any", _) -> fail "Nothing is allowed here. Sorry."
      _ -> return ()
    validateTypeDisallowed (Choice2of2 s) = assert (not . isNothing $ validate s val) $ "value disallowed"

assert :: Bool -> String -> Parser ()
assert True _ = return ()
assert False e = fail e

maybeCheck :: (a -> Parser ()) -> Maybe a -> Parser ()
maybeCheck p (Just a) = p a
maybeCheck _ _ = return ()

validateString :: Schema String -> Text -> Parser ()
validateString schema str = do
  let checkMinLength l = assert (length str >= l) $ "length of string must be at least " ++ show l
  checkMinLength $ schemaMinLength schema
  let checkMaxLength l = assert (length str <= l) $ "length of string must be at most " ++ show l
  maybeCheck checkMaxLength (schemaMaxLength schema)
  let checkPattern (Pattern source compiled) = assert (match compiled $ unpack str) $ "string must match pattern " ++ show source
  maybeCheck checkPattern $ schemaPattern schema
  let checkFormat format = case format of
        "date-time" -> return ()
        "data" -> return ()
        "time" -> return ()
        "utc-millisec" -> return ()
        "regex" -> void (makeRegexM (unpack str) :: Parser Regex)
        "color" -> return () -- not going to implement this
        "style" -> return () -- not going to implement this
        "phone" -> return ()
        "uri" -> return ()
        "email" -> return ()
        "ip-address" -> return ()
        "ipv6" -> return ()
        "host-name" -> return ()
        _ -> return () -- unknown format
  maybeCheck checkFormat $ schemaFormat schema

validateNumber :: Schema String -> Number -> Parser ()
validateNumber schema num = do
  let checkMinimum m = if schemaExclusiveMinimum schema
                       then assert (num > m)  $ "number must be greater than " ++ show m
                       else assert (num >= m) $ "number must be greater than or equal " ++ show m
  maybeCheck checkMinimum $ schemaMinimum schema
  let checkMaximum m = if schemaExclusiveMaximum schema
                       then assert (num < m)  $ "number must be less than " ++ show m
                       else assert (num <= m) $ "number must be less than or equal " ++ show m
  maybeCheck checkMaximum $ schemaMaximum schema
  let checkDivisibleBy devisor = assert (num `isDivisibleBy` devisor) $ "number must be devisible by " ++ show devisor
  maybeCheck checkDivisibleBy $ schemaDivisibleBy schema
  where
    isDivisibleBy :: Number -> Number -> Bool
    isDivisibleBy (I i) (I j) = i `mod` j == 0
    isDivisibleBy a b = a == fromInteger 0 || denominator (approxRational (a / b) epsilon) `elem` [-1,1]
      where epsilon = D $ 10 ** (-10)

validateObject :: Schema String -> A.Object -> Parser ()
validateObject schema obj = do
  forM_ (H.toList obj) $ \(k, v) -> do
    let maybeProperty = H.lookup k (schemaProperties schema)
    maybeCheck (\propSchema -> validateP propSchema v) maybeProperty
    let patternProps = filter (flip match (unpack k) . patternCompiled . fst) $ schemaPatternProperties schema
    forM_ patternProps $ flip validateP v . snd
    let checkAdditionalProperties ap = case ap of
          Choice1of3 _ -> fail "not implemented"
          Choice2of3 b -> assert b $ "additional property " ++ unpack k ++ " is not allowed"
          Choice3of3 s -> validateP s v
    when (isNothing maybeProperty && L.null patternProps) $ do
      checkAdditionalProperties (schemaAdditionalProperties schema)
    let checkDependencies deps = case deps of
          Choice1of2 props -> forM_ props $ \prop -> case H.lookup prop obj of
            Nothing -> fail $ "property " ++ unpack k ++ " depends on property " ++ show prop
            Just _ -> return ()
          Choice2of2 depSchema -> validateP depSchema (A.Object obj)
    let maybeDependencies = H.lookup k (schemaDependencies schema)
    maybeCheck checkDependencies maybeDependencies
  let requiredProps = map fst . filter (schemaRequired . snd) . H.toList $ schemaProperties schema
  forM_ requiredProps $ \prop -> case H.lookup prop obj of
    Nothing -> fail $ "required property " ++ unpack prop ++ " is missing"
    Just _ -> return ()

validateArray :: Schema String -> A.Array -> Parser ()
validateArray schema arr = do
  let len = V.length arr
  let list = V.toList arr
  let checkMinItems m = assert (len >= m) $ "array must have at least " ++ show m ++ " items"
  checkMinItems $ schemaMinItems schema
  let checkMaxItems m = assert (len <= m) $ "array must have at most " ++ show m ++ " items"
  maybeCheck checkMaxItems $ schemaMaxItems schema
  let checkUnique = assert (L.length (L.nub list) == len) "all array items must be unique"
  if schemaUniqueItems schema then checkUnique else return ()
  let checkItems items = case items of
        Choice1of3 _ -> fail "not implemented"
        Choice2of3 s -> assert (V.all (isNothing . validate s) arr) "all items in the array must validate against the schema given in 'items'"
        Choice3of3 ss -> do
          sequence_ $ zipWith validateP ss list
          let additionalItems = drop (L.length ss) list
          let checkAdditionalItems ai = case ai of
                Choice1of3 _ -> fail "not implemented"
                Choice2of3 b -> assert (b || L.null additionalItems) $ "no additional items allowed"
                Choice3of3 additionalSchema -> sequence_ $ map (validateP additionalSchema) additionalItems
          checkAdditionalItems $ schemaAdditionalItems schema
  maybeCheck checkItems $ schemaItems schema
