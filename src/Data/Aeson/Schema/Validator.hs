{-# LANGUAGE RankNTypes #-}

module Data.Aeson.Schema.Validator
  ( ValidationError
  , validate
  ) where

import           Data.Aeson                (Value (..))
import qualified Data.Aeson                as A
import qualified Data.HashMap.Strict       as H
import qualified Data.List                 as L
import qualified Data.Map                  as M
import           Data.Maybe                (isNothing)
import           Data.Scientific           (Scientific, isInteger)
import           Data.Text                 (Text, length, unpack)
import qualified Data.Vector               as V
import           Prelude                   hiding (foldr, length)
import           Text.Regex.Base           (match)

import           Data.Aeson.Schema.Types
import           Data.Aeson.Schema.Choice
import           Data.Aeson.Schema.Helpers

-- | Errors encountered during validation
type ValidationError = String

validationError :: ValidationError -> [ValidationError]
validationError e = [e]

valid :: [ValidationError]
valid = []

-- | Validates a JSON value against a schema.
validate :: Ord ref
         => Graph Schema ref -- ^ referenced schemas
         -> Schema ref
         -> Value
         -> [ValidationError]
validate graph schema val = case schemaDRef schema of
  Just ref -> case M.lookup ref graph of
    Nothing -> validationError "referenced schema is not in map"
    Just referencedSchema -> validate graph referencedSchema val
  Nothing -> L.concat
    [ case schemaType schema of
        [t] -> validateType t
        ts  -> if L.any L.null (map validateType ts) then [] else validationError "no type matched"
    , maybeCheck checkEnum $ schemaEnum schema
    , concatMap validateTypeDisallowed (schemaDisallow schema)
    , concatMap (flip (validate graph) val) (schemaExtends schema)
    ]
  where
    validateType (Choice1of2 t) = case (t, val) of
      (StringType, String str) -> validateString schema str
      (NumberType, Number num) -> validateNumber schema num
      (IntegerType, Number num) -> validateInteger schema num
      (BooleanType, Bool _) -> valid
      (ObjectType, Object obj) -> validateObject graph schema obj
      (ArrayType, Array arr) -> validateArray graph schema arr
      (NullType, Null) -> valid
      (AnyType, _) -> case val of
        String str -> validateString schema str
        Number num -> validateNumber schema num
        Object obj -> validateObject graph schema obj
        Array arr  -> validateArray graph schema arr
        _ -> valid
      (typ, _) -> validationError $ "type mismatch: expected " ++ show typ ++ " but got " ++ getType val
    validateType (Choice2of2 s) = validate graph s val

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
    isType (Number num) IntegerType = isInteger num
    isType (Number _) NumberType = True
    isType (Bool _) BooleanType = True
    isType (Object _) ObjectType = True
    isType (Array _) ArrayType = True
    isType _ AnyType = True
    isType _ _ = False

    validateTypeDisallowed (Choice1of2 t) = if isType val t
        then validationError $ "values of type " ++ show t ++ " are not allowed here"
        else valid
    validateTypeDisallowed (Choice2of2 s) = assert (not . L.null $ validate graph s val) "value disallowed"

assert :: Bool -> String -> [ValidationError]
assert True _ = valid
assert False e = validationError e

maybeCheck :: (a -> [ValidationError]) -> Maybe a -> [ValidationError]
maybeCheck p (Just a) = p a
maybeCheck _ _ = valid

validateString :: Schema ref -> Text -> [ValidationError]
validateString schema str = L.concat
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

validateNumber :: Schema ref -> Scientific -> [ValidationError]
validateNumber schema num = L.concat
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

validateInteger :: Schema ref -> Scientific -> [ValidationError]
validateInteger schema num =
  assert (isInteger num) "number must be an integer" ++
  validateNumber schema num

validateObject :: Ord ref => Graph Schema ref -> Schema ref -> A.Object -> [ValidationError]
validateObject graph schema obj =
  concatMap (uncurry checkKeyValue) (H.toList obj) ++
  concatMap checkRequiredProperty requiredProperties
  where
    checkKeyValue k v = L.concat
      [ maybeCheck (flip (validate graph) v) property
      , concatMap (flip (validate graph) v . snd) matchingPatternsProperties
      , if isNothing property && L.null matchingPatternsProperties
        then checkAdditionalProperties (schemaAdditionalProperties schema)
        else valid
      , maybeCheck checkDependencies $ H.lookup k (schemaDependencies schema)
      ]
      where
        property = H.lookup k (schemaProperties schema)
        matchingPatternsProperties = filter (flip match (unpack k) . patternCompiled . fst) $ schemaPatternProperties schema
        checkAdditionalProperties ap = case ap of
          Choice1of2 b -> assert b $ "additional property " ++ unpack k ++ " is not allowed"
          Choice2of2 s -> validate graph s v
        checkDependencies deps = case deps of
          Choice1of2 props -> L.concat $ flip map props $ \prop -> case H.lookup prop obj of
            Nothing -> validationError $ "property " ++ unpack k ++ " depends on property " ++ show prop
            Just _ -> valid
          Choice2of2 depSchema -> validate graph depSchema (Object obj)
    requiredProperties = map fst . filter (schemaRequired . snd) . H.toList $ schemaProperties schema
    checkRequiredProperty key = case H.lookup key obj of
      Nothing -> validationError $ "required property " ++ unpack key ++ " is missing"
      Just _ -> valid

validateArray :: Ord ref => Graph Schema ref -> Schema ref -> A.Array -> [ValidationError]
validateArray graph schema arr = L.concat
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
      Choice1of2 s -> assert (V.all (L.null . validate graph s) arr) "all items in the array must validate against the schema given in 'items'"
      Choice2of2 ss ->
        let additionalItems = drop (L.length ss) list
            checkAdditionalItems ai = case ai of
              Choice1of2 b -> assert (b || L.null additionalItems) "no additional items allowed"
              Choice2of2 additionalSchema -> concatMap (validate graph additionalSchema) additionalItems
        in L.concat (zipWith (validate graph) ss list) ++
           checkAdditionalItems (schemaAdditionalItems schema)

