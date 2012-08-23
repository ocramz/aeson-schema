{-# LANGUAGE RankNTypes #-}

module Data.Aeson.Schema.Validator
  ( Validator (..)
  , validate
  ) where

import Prelude hiding (foldr, length)
import Data.Maybe (isNothing)
import qualified Data.List as L
import Control.Monad (msum)
import Data.Aeson (Value (..))
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Data.Text (Text, unpack, length)
import Data.Attoparsec.Number (Number (..))
import Text.Regex.PCRE (match)

import Data.Aeson.Schema
import Data.Aeson.Schema.Helpers
import Data.Aeson.Schema.Choice

type ValidationError = String
type SchemaValidator = forall v. Validator v => v ValidationError

class Validator v where
  validationError :: a -> v a
  valid :: v a
  isValid :: v a -> Bool
  allValid :: [v a] -> v a
  anyValid :: a -> [v b] -> v a
  anyValid err vs = if L.any isValid vs then valid else validationError err

instance Validator [] where
  validationError e = [e]
  valid = []
  isValid = L.null
  allValid = L.concat

instance Validator Maybe where
  validationError = Just
  valid = Nothing
  isValid = isNothing
  allValid = msum

type RecursiveSchema v = Schema v (Fix (Schema v))

validate :: RecursiveSchema V3 -> Value -> SchemaValidator
validate schema val = case schemaDRef schema of
  Just (Fix referencedSchema) -> validate referencedSchema val
  Nothing -> allValid
    [ anyValid "no type matched" $ map validateType (schemaType schema)
    , maybeCheck checkEnum $ schemaEnum schema
    , allValid $ map validateTypeDisallowed (schemaDisallow schema)
    , allValid $ map (flip validate val) (schemaExtends schema)
    ]
  where
    validateType :: Choice2 SchemaType (RecursiveSchema V3) -> SchemaValidator
    validateType (Choice1of2 t) = case (t, val) of
      (StringType, String str) -> validateString schema str
      (NumberType, Number num) -> validateNumber schema num
      (IntegerType, Number num@(I _)) -> validateNumber schema num
      (BooleanType, Bool _) -> valid
      (ObjectType, Object obj) -> validateObject schema obj
      (ArrayType, Array arr) -> validateArray schema arr
      (NullType, Null) -> valid
      (AnyType, _) -> case val of
        String str -> validateString schema str
        Number num -> validateNumber schema num
        Object obj -> validateObject schema obj
        Array arr  -> validateArray schema arr
        _ -> valid
      (typ, _) -> validationError $ "type mismatch: expected " ++ show typ ++ " but got " ++ getType val
    validateType (Choice2of2 s) = validate s val

    getType :: A.Value -> String
    getType (String _) = "string"
    getType (Number _) = "number"
    getType (Bool _)   = "boolean"
    getType (Object _) = "object"
    getType (Array _)  = "array"
    getType Null       = "null"

    checkEnum e = assert (val `elem` e) "value has to be one of the values in enum"

    isType :: Value -> SchemaType -> Bool
    isType (String _) StringType = True
    isType (Number (I _)) IntegerType = True
    isType (Number _) NumberType = True
    isType (Bool _) BooleanType = True
    isType (Object _) ObjectType = True
    isType (Array _) ArrayType = True
    isType _ AnyType = True
    isType _ _ = False

    validateTypeDisallowed :: Choice2 SchemaType (RecursiveSchema V3) -> SchemaValidator
    validateTypeDisallowed (Choice1of2 t) = if isType val t
        then validationError $ "values of type " ++ show t ++ " are not allowed here"
        else valid
    validateTypeDisallowed (Choice2of2 s) = assert (not . isNothing $ validate s val) $ "value disallowed"

assert :: Bool -> String -> SchemaValidator
assert True _ = valid
assert False e = validationError e

maybeCheck :: (a -> SchemaValidator) -> Maybe a -> SchemaValidator
maybeCheck p (Just a) = p a
maybeCheck _ _ = valid

validateString :: RecursiveSchema V3 -> Text -> SchemaValidator
validateString schema str = allValid
  [ checkMinLength $ schemaMinLength schema
  , maybeCheck checkMaxLength (schemaMaxLength schema)
  , maybeCheck checkPattern $ schemaPattern schema
  , maybeCheck checkFormat $ schemaFormat schema
  ]
  where
    checkMinLength l = assert (length str >= l) $ "length of string must be at least " ++ show l
    checkMaxLength l = assert (length str <= l) $ "length of string must be at most " ++ show l
    checkPattern (Pattern source compiled) = assert (match compiled $ unpack str) $ "string must match pattern " ++ show source
    checkFormat format = maybe valid validationError $ validateFormat format str

validateNumber :: RecursiveSchema V3 -> Number -> SchemaValidator
validateNumber schema num = allValid
  [ maybeCheck (checkMinimum $ schemaExclusiveMinimum schema) $ schemaMinimum schema
  , maybeCheck (checkMaximum $ schemaExclusiveMaximum schema) $ schemaMaximum schema
  , maybeCheck checkDivisibleBy $ schemaDivisibleBy schema
  ]
  where
    checkMinimum excl m = if excl
      then assert (num > m)  $ "number must be greater than " ++ show m
      else assert (num >= m) $ "number must be greater than or equal " ++ show m
    checkMaximum excl m = if excl
      then assert (num < m)  $ "number must be less than " ++ show m
      else assert (num <= m) $ "number must be less than or equal " ++ show m
    checkDivisibleBy devisor = assert (num `isDivisibleBy` devisor) $ "number must be devisible by " ++ show devisor

validateObject :: RecursiveSchema V3 -> A.Object -> SchemaValidator
validateObject schema obj = allValid
  [ allValid $ map (uncurry checkKeyValue) (H.toList obj)
  , allValid $ map checkRequiredProperty requiredProperties
  ]
  where
    checkKeyValue k v = allValid
      [ maybeCheck (flip validate v) property
      , allValid $ map (flip validate v . snd) matchingPatternsProperties
      , if (isNothing property && L.null matchingPatternsProperties)
        then checkAdditionalProperties (schemaAdditionalProperties schema)
        else valid
      , maybeCheck checkDependencies $ H.lookup k (schemaDependencies schema)
      ]
      where
        property = H.lookup k (schemaProperties schema)
        matchingPatternsProperties = filter (flip match (unpack k) . patternCompiled . fst) $ schemaPatternProperties schema
        checkAdditionalProperties ap = case ap of
          Choice1of2 b -> assert b $ "additional property " ++ unpack k ++ " is not allowed"
          Choice2of2 s -> validate s v
        checkDependencies deps = case deps of
          Choice1of2 props -> allValid $ flip map props $ \prop -> case H.lookup prop obj of
            Nothing -> validationError $ "property " ++ unpack k ++ " depends on property " ++ show prop
            Just _ -> valid
          Choice2of2 depSchema -> validate depSchema (Object obj)
    requiredProperties = map fst . filter (schemaRequired . snd) . H.toList $ schemaProperties schema
    checkRequiredProperty key = case H.lookup key obj of
      Nothing -> validationError $ "required property " ++ unpack key ++ " is missing"
      Just _ -> valid

validateArray :: RecursiveSchema V3 -> A.Array -> SchemaValidator
validateArray schema arr = allValid
  [ checkMinItems $ schemaMinItems schema
  , maybeCheck checkMaxItems $ schemaMaxItems schema
  , if schemaUniqueItems schema then checkUnique else valid
  , maybeCheck checkItems $ schemaItems schema
  ]
  where
    len = V.length arr
    list = V.toList arr
    checkMinItems m = assert (len >= m) $ "array must have at least " ++ show m ++ " items"
    checkMaxItems m = assert (len <= m) $ "array must have at most " ++ show m ++ " items"
    checkUnique = assert (vectorUnique arr) "all array items must be unique"
    checkItems items = case items of
      Choice1of2 s -> assert (V.all (isNothing . validate s) arr) "all items in the array must validate against the schema given in 'items'"
      Choice2of2 ss ->
        let additionalItems = drop (L.length ss) list
            checkAdditionalItems ai = case ai of
              Choice1of2 b -> assert (b || L.null additionalItems) $ "no additional items allowed"
              Choice2of2 additionalSchema -> allValid $ map (validate additionalSchema) additionalItems
        in allValid [ allValid $ zipWith validate ss list
                    , checkAdditionalItems $ schemaAdditionalItems schema
                    ]
