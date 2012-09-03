{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, TupleSections, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Data.Aeson.Schema.CodeGen
  ( generate
  ) where

import Control.Monad (forM_, when)
import Control.Arrow (second)
import Data.Function (on)
import Data.Char (isAlphaNum, isLetter, toLower, toUpper)
import Data.Attoparsec.Number (Number (..))
import Control.Monad.RWS.Lazy (RWST (..), MonadReader (..), MonadWriter (..), MonadState (..))
import qualified Control.Monad.Trans.Class as MT
import Control.Applicative (Applicative (..), (<$>), (<*>), (<|>))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.String (IsString (..))
import Data.List (unzip4, nub)
import Data.Monoid ((<>))
import Data.Either (rights)
import Data.Text (Text, pack, unpack)
import Data.Maybe (catMaybes, isNothing, maybeToList)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Traversable (forM, traverse)
import qualified Text.Regex.PCRE as PCRE
import Text.Regex.PCRE.String (Regex)
import Data.Aeson
import Data.Aeson.Types (Parser, parse)
import Data.Generics (everywhere, mkT, everything, mkQ)

import Data.Aeson.Schema.Helpers
import Data.Aeson.Schema
import Data.Aeson.Schema.Validator
import Data.Aeson.Schema.Choice


type Code = [Either Text Dec]
type StringSet = HS.HashSet String
type SchemaGraph = Graph (Schema V3) Text

newtype CodeGenM a = CodeGenM
  { unCodeGenM :: RWST SchemaGraph Code StringSet Q a
  } deriving (Monad, Applicative, Functor, MonadReader SchemaGraph, MonadWriter Code, MonadState StringSet)

instance Quasi CodeGenM where
  qNewName s = do
    used <- get
    let free = head $ dropWhile (`HS.member` used) $ (if validName s then (s:) else id) $ map (\i -> s ++ "_" ++ show i) ([1..] :: [Int])
    put $ HS.insert free used
    return $ Name (mkOccName free) NameS
    where
      validName s = not (s `elem` ["", "_"])
  qReport b = CodeGenM . MT.lift . report b
  qRecover (CodeGenM handler) (CodeGenM action) = do
    graph <- ask
    currState <- get
    (a, s, w) <- CodeGenM $ MT.lift $ (recover `on` \m -> runRWST m graph currState) handler action
    put s
    tell w
    return a
  qLookupName b = CodeGenM . MT.lift . (if b then lookupTypeName else lookupValueName)
  qReify = CodeGenM . MT.lift . reify
  qReifyInstances name = CodeGenM . MT.lift . reifyInstances name
  qLocation = CodeGenM . MT.lift $ location
  qRunIO = CodeGenM . MT.lift . runIO
  qAddDependentFile = CodeGenM . MT.lift . addDependentFile

instance IsString Name where
  fromString = mkName

instance Lift Text where
  lift txt = [| pack $(lift (unpack txt)) |]

instance Lift Double where
  lift d = [| fromRational $(litE . rationalL . toRational $ d) :: Double |]

instance Lift Number where
  lift (I i) = [| I i |]
  lift (D d) = [| D d |]

instance Lift SchemaType where
  lift StringType  = [| StringType |]
  lift NumberType  = [| NumberType |]
  lift IntegerType = [| IntegerType |]
  lift BooleanType = [| BooleanType |]
  lift ObjectType  = [| ObjectType |]
  lift ArrayType   = [| ArrayType |]
  lift NullType    = [| NullType |]
  lift AnyType     = [| AnyType |]

instance (Lift k, Lift v) => Lift (HM.HashMap k v) where
  lift hm = [| HM.fromList $(lift (HM.toList hm)) |]

instance (Lift a) => Lift (V.Vector a) where
  lift vec = [| V.fromList $(lift (V.toList vec)) |]

instance Lift Value where
  lift (Object o) = [| Object o |]
  lift (Array a)  = [| Array a |]
  lift (String t) = [| String t |]
  lift (Number n) = [| Number n |]
  lift (Bool b)   = [| Bool b |]
  lift Null       = [| Null |]

instance (Lift a, Lift b) => Lift (Choice2 a b) where
  lift (Choice1of2 a) = [| Choice1of2 a |]
  lift (Choice2of2 b) = [| Choice2of2 b |]

instance (Lift a, Lift b, Lift c) => Lift (Choice3 a b c) where
  lift (Choice1of3 a) = [| Choice1of3 a |]
  lift (Choice2of3 b) = [| Choice2of3 b |]
  lift (Choice3of3 c) = [| Choice2of3 c |]

instance Lift Pattern where
  lift (Pattern src _) = [| let Right p = mkPattern src in p |]

instance Lift (Schema V3 Text) where
  lift schema = recConE ''Schema
    [ field 'schemaType $ schemaType schema
    , field 'schemaProperties $ schemaProperties schema
    , field 'schemaPatternProperties $ schemaPatternProperties schema
    , field 'schemaAdditionalProperties $ schemaAdditionalProperties schema
    , field 'schemaItems $ schemaItems schema
    , field 'schemaAdditionalItems $ schemaAdditionalItems schema
    , field 'schemaRequired $ schemaRequired schema
    , field 'schemaDependencies $ schemaDependencies schema
    , field 'schemaMinimum $ schemaMinimum schema
    , field 'schemaMaximum $ schemaMaximum schema
    , field 'schemaExclusiveMinimum $ schemaExclusiveMinimum schema
    , field 'schemaExclusiveMaximum $ schemaExclusiveMaximum schema
    , field 'schemaMinItems $ schemaMinItems schema
    , field 'schemaMaxItems $ schemaMaxItems schema
    , field 'schemaUniqueItems $ schemaUniqueItems schema
    , field 'schemaPattern $ schemaPattern schema
    , field 'schemaMinLength $ schemaMinLength schema
    , field 'schemaMaxLength $ schemaMaxLength schema
    , field 'schemaEnum $ schemaEnum schema
    , field 'schemaEnumDescriptions $ schemaEnumDescriptions schema
    , field 'schemaDefault $ schemaDefault schema
    , field 'schemaTitle $ schemaTitle schema
    , field 'schemaDescription $ schemaDescription schema
    , field 'schemaFormat $ schemaFormat schema
    , field 'schemaDivisibleBy $ schemaDivisibleBy schema
    , field 'schemaDisallow $ schemaDisallow schema
    , field 'schemaExtends $ schemaExtends schema
    , field 'schemaId $ schemaId schema
    , field 'schemaDRef $ schemaDRef schema
    , field 'schemaDSchema $ schemaDSchema schema
    ]
    where field name val = (name,) <$> lift val

instance (Lift k, Lift v) => Lift (M.Map k v) where
  lift m = [| M.fromList $(lift $ M.toList m) |]

replaceHiddenModules :: Dec -> Dec
replaceHiddenModules = everywhere $ mkT replaceModule 
  where
    replacements =
      [ ("Data.HashMap.Base", "Data.HashMap.Lazy")
      , ("Data.Aeson.Types.Class", "Data.Aeson")
      , ("Data.Aeson.Types.Internal", "Data.Aeson")
      , ("GHC.Integer.Type", "Prelude") -- "Could not find module `GHC.Integer.Type'; it is a hidden module in the package `integer-gmp'"
      ]
    replaceModule :: Name -> Name
    replaceModule n = case nameModule n of
      Just "GHC.Tuple" -> mkName $ nameBase n
      Just m -> case lookup m replacements of
        Just r -> mkName $ r ++ ('.' : nameBase n)
        Nothing -> n
      _ -> n

extraModules :: [String]
extraModules =
  [ "Text.Regex" -- provides RegexMaker instances
  , "Text.Regex.PCRE.String" -- provides RegexLike instances, Regex type
  , "Data.Aeson.Types" -- Parser type
  ]

getUsedModules :: [Dec] -> [String]
getUsedModules = nub . concatMap (everything (++) ([] `mkQ` extractModule))
  where
    extractModule :: Name -> [String]
    extractModule = maybeToList . nameModule

generate :: Graph (Schema V3) Text -> Q Code
generate graph = do
  ((), _, code) <- runRWST (unCodeGenM generateTopLevel) graph HS.empty
  let code' = map (mapEither id replaceHiddenModules) code
  let mods = extraModules ++ getUsedModules (rights code')
  let imprts = map (\m -> "import " <> pack m) mods
  return $ map Left imprts ++ code'
  where
    mapEither :: (a1 -> a2) -> (b1 -> b2) -> Either a1 b1 -> Either a2 b2
    mapEither f _ (Left l) = Left (f l)
    mapEither _ f (Right r) = Right (f r)

generateTopLevel :: CodeGenM ()
generateTopLevel = do
  graph <- ask
  graphN <- qNewName "graph"
  when (nameBase graphN /= "graph") $ fail "name graph is already taken"
  graphDecType <- runQ $ sigD graphN $ conT ''M.Map `appT` conT ''Text `appT` (conT ''Schema `appT` conT ''V3 `appT` conT ''Text)
  graphDec <- runQ $ valD (varP graphN) (normalB $ lift graph) []
  tell [Right graphDecType, Right graphDec]
  forM_ (M.toList graph) $ uncurry generateSchema

generateSchema :: Text -> Schema V3 Text -> CodeGenM (TypeQ, ExpQ)
generateSchema name schema = case schemaDRef schema of
  Just ref -> ask >>= \graph -> case M.lookup ref graph of
    Nothing -> fail "couldn't find referenced schema"
    Just referencedSchema -> generateSchema name referencedSchema -- TODO
  Nothing -> second wrap <$> case schemaType schema of
    [] -> fail "empty type"
    [Choice1of2 typ] -> generateSimpleType name typ
    [Choice2of2 sch] -> generateSchema name sch
    unionType -> do
      let l = pack . show $ length unionType
      let names = map (\i -> name <> "Choice" <> pack (show i) <> "of" <> l) ([1..] :: [Int])
      subs <- sequence $ zipWith (choice2 (flip generateSimpleType) (flip generateSchema)) unionType names
      generateUnionType subs
  where
    generateSimpleType :: Text -> SchemaType -> CodeGenM (TypeQ, ExpQ)
    generateSimpleType name' typ = case typ of
      StringType  -> generateString schema
      NumberType  -> generateNumber schema
      IntegerType -> generateInteger schema
      BooleanType -> generateBoolean
      ObjectType  -> generateObject name' schema
      ArrayType   -> generateArray name' schema
      NullType    -> generateNull
      AnyType     -> generateAny schema
    generateUnionType :: [(TypeQ, ExpQ)] -> CodeGenM (TypeQ, ExpQ)
    generateUnionType union = return (typ, lamE [varP val] code)
      where
        n = length union
        unionParsers = zipWith (\i parser -> [| $(choiceConE i n) <$> $parser $(varE val) |]) [1..] (map snd union)
        choiceConE :: Int -> Int -> ExpQ
        choiceConE i j = conE $ mkName $ "Data.Aeson.Schema.Choice.Choice" ++ show i ++ "of" ++ show j
        choiceT i = conT $ mkName $ "Data.Aeson.Schema.Choice.Choice" ++ show i
        typ = foldl appT (choiceT n) $ map fst union
        code = foldr (\choiceParser unionParser -> [| $choiceParser <|> $unionParser |]) [| fail "no type in union" |] unionParsers
    val = mkName "val"
    checkEnum xs = assertStmt [| $(varE val) `elem` xs |] "not one of the values in enum"
    checkDisallow dis = noBindS $ doE $ map (noBindS . choice2 disallowType disallowSchema) dis
    disallowType StringType  = disallowPattern (conP 'String [wildP]) "strings are disallowed"
    disallowType NumberType  = disallowPattern (conP 'Number [wildP]) "numbers are disallowed"
    disallowType IntegerType = disallowPattern (conP 'Number [conP 'I [wildP]]) "integers are disallowed"
    disallowType BooleanType = disallowPattern (conP 'Bool [wildP]) "booleans are disallowed"
    disallowType ObjectType  = disallowPattern (conP 'Object [wildP]) "objects are disallowed"
    disallowType ArrayType   = disallowPattern (conP 'Array [wildP]) "arrays are disallowed"
    disallowType NullType    = disallowPattern (conP 'Null []) "null is disallowed"
    disallowType AnyType     = [| fail "Nothing is allowed here. Sorry." |]
    disallowPattern pat err = caseE (varE val)
      [ match pat (normalB [| fail err |])[]
      , match wildP (normalB [| return () |]) []
      ]
    disallowSchema sch = caseE [| validate $(varE $ mkName "graph") $(lift sch) $(varE val) |]
      [ match (conP 'Just [wildP]) (normalB [| return () |]) []
      , match wildP (normalB [| fail "disallowed" |]) []
      ]
    checkExtends exts = noBindS $ doE $ flip map exts $ noBindS . \sch -> caseE [| validate $(varE $ mkName "graph") $(lift sch) $(varE val) |]
      [ match (conP 'Just [varP "err"]) (normalB [| fail $(varE "err") |]) []
      , match wildP (normalB [| return () |]) []
      ]
    checkers = catMaybes
      [ checkEnum <$> schemaEnum schema
      , if null (schemaDisallow schema) then Nothing else Just (checkDisallow $ schemaDisallow schema)
      , if null (schemaExtends schema) then Nothing else Just (checkExtends $ schemaExtends schema)
      ]
    wrap parser = if null checkers
      then parser
      else lamE [varP val] $ doE $ checkers ++ [noBindS $ parser `appE` varE val]

assertStmt :: ExpQ -> String -> StmtQ
assertStmt expr err = noBindS [| when $(expr) (fail err) |]

lambdaPattern :: PatQ -> ExpQ -> ExpQ -> ExpQ
lambdaPattern pat body err = lamE [varP val] $ caseE (varE val)
  [ match pat (normalB body) []
  , match wildP (normalB err) []
  ]
  where val = mkName "val"

returnE :: ExpQ -> ExpQ
returnE r = [| return $(r) |]

generateString :: Schema V3 Text -> CodeGenM (TypeQ, ExpQ)
generateString schema = return (conT ''Text, code)
  where
    str = mkName "str"
    checkMinLength l = assertStmt [| T.length $(varE str) >= l |] $ "string must have at least " ++ show l ++ " characters"
    checkMaxLength l = assertStmt [| T.length $(varE str) <= l |] $ "string must have at most " ++ show l ++ " characters"
    checkPattern (Pattern p _) = noBindS $ doE
      [ bindS (varP "regex") [| PCRE.makeRegexM $(lift (T.unpack p)) |]
      , assertStmt [| PCRE.match ($(varE "regex") :: Regex) (unpack $(varE str)) |] $ "string must match pattern " ++ show p
      ]
    checkFormat format = noBindS [| maybe (return ()) fail (validateFormat $(lift format) $(varE str)) |]
    checkers = catMaybes
      [ if schemaMinLength schema > 0 then Just (checkMinLength $ schemaMinLength schema) else Nothing
      , checkMaxLength <$> schemaMaxLength schema
      , checkPattern <$> schemaPattern schema
      , checkFormat <$> schemaFormat schema
      ]
    code = lambdaPattern (conP 'String [varP str])
                         (doE $ checkers ++ [noBindS [| return $(varE str) |]])
                         [| fail "not a string" |]

generateNumber :: Schema V3 Text -> CodeGenM (TypeQ, ExpQ)
generateNumber schema = return (conT ''Number, code)
  where
    num = mkName "num"
    code = lambdaPattern (conP 'Number [varP num])
                         (doE $ numberCheckers num schema ++ [noBindS [| return $(varE num) |]])
                         [| fail "not a number" |]

generateInteger :: Schema V3 Text -> CodeGenM (TypeQ, ExpQ)
generateInteger schema = return (conT ''Integer, code)
  where
    num = mkName "num"
    code = lambdaPattern (conP 'Number [asP num $ conP "I" [varP "i"]])
                         (doE $ numberCheckers num schema ++ [noBindS [| return $(varE "i") |]])
                         [| fail "not an integer" |]

numberCheckers :: Name -> Schema V3 Text -> [StmtQ]
numberCheckers num schema = catMaybes
  [ checkMinimum (schemaExclusiveMinimum schema) <$> schemaMinimum schema
  , checkMaximum (schemaExclusiveMaximum schema) <$> schemaMaximum schema
  , checkDivisibleBy <$> schemaDivisibleBy schema
  ]
  where
    checkMinimum, checkMaximum :: Bool -> Number -> StmtQ
    checkMinimum excl m = if excl
      then assertStmt [| $(varE num) >  m |] $ "number must be greater than " ++ show m
      else assertStmt [| $(varE num) >= m |] $ "number must be greater than or equal " ++ show m
    checkMaximum excl m = if excl
      then assertStmt [| $(varE num) <  m |] $ "number must be less than " ++ show m
      else assertStmt [| $(varE num) <= m |] $ "number must be less than or equal " ++ show m
    checkDivisibleBy devisor = assertStmt [| $(varE num) `isDivisibleBy` devisor |] $ "number must be devisible by " ++ show devisor

generateBoolean :: CodeGenM (TypeQ, ExpQ)
generateBoolean = return (conT ''Bool, varE 'parseJSON)

generateNull :: CodeGenM (TypeQ, ExpQ)
generateNull = return (tupleT 0, code)
  where
    code = lambdaPattern (conP 'Null [])
                         [| return () |]
                         [| fail "not null" |]

cleanName :: String -> String
cleanName str = charFirst
  where
    isAllowed c = isAlphaNum c || c `elem` "'_"
    cleaned = filter isAllowed str
    charFirst = case cleaned of
      "" -> ""
      (chr:rest) -> if isLetter chr || chr == '_' then cleaned else '_':cleaned

firstUpper, firstLower :: String -> String
firstUpper "" = ""
firstUpper (c:cs) = toUpper c : cs
firstLower "" = ""
firstLower (c:cs) = toLower c : cs

generateObject :: Text -> Schema V3 Text -> CodeGenM (TypeQ, ExpQ)
generateObject name schema = do
  let propertiesList = HM.toList $ schemaProperties schema
  (propertyNames, propertyTypes, propertyParsers, defaultParsers) <- fmap unzip4 $ forM propertiesList $ \(fieldName, propertySchema) -> do
    let cleanedFieldName = cleanName $ unpack fieldName
    propertyName <- qNewName $ firstLower $ cleanedFieldName
    (typ, expr) <- generateSchema (name <> pack (firstUpper cleanedFieldName)) propertySchema
    let lookupProperty = [| HM.lookup $(lift fieldName) $(varE obj) |]
    case schemaDefault propertySchema of
      Just defaultValue -> do
        defaultName <- qNewName $ "default" <> firstUpper cleanedFieldName
        return (propertyName, typ, [| maybe (return $(varE defaultName)) $expr $lookupProperty |], Just $ valD (conP 'Success [varP defaultName]) (normalB [| parse $expr $(lift defaultValue) |]) [])
      Nothing -> return $ if schemaRequired propertySchema
        then (propertyName, typ, [| maybe (fail $(lift $ "required property " ++ unpack fieldName ++ " missing")) $expr $lookupProperty |], Nothing)
        else (propertyName, conT ''Maybe `appT` typ, [| traverse $expr $lookupProperty |], Nothing)
  conName <- qNewName $ unpack name
  let typ = conT conName
  let dataCon = recC conName $ zipWith (\name typ -> (name,NotStrict,) <$> typ) propertyNames propertyTypes
  dataDec <- runQ $ dataD (cxt []) conName [] [dataCon] []
  let parser = foldl (\parser propertyParser -> [| $parser <*> $propertyParser |]) [| pure $(conE conName) |] propertyParsers
  fromJSONInst <- runQ $ instanceD (cxt []) (conT ''FromJSON `appT` typ)
    [ funD (mkName "parseJSON") -- cannot use a qualified name here
        [ clause [conP 'Object [varP obj]] (normalB $ doE $ checkers ++ [noBindS parser]) (catMaybes defaultParsers)
        , clause [wildP] (normalB [| fail "not an object" |]) []
        ]
    ]
  tell [Right dataDec, Right fromJSONInst]
  return (typ, [| parseJSON |])
  where
    obj = mkName "obj"
    checkDependencies deps = noBindS
      [| let items = HM.toList $(varE obj) in forM_ items $ \(name, _) -> case HM.lookup name $(lift deps) of
           Nothing -> return ()
           Just (Choice1of2 props) -> forM_ props $ \prop -> if isNothing (HM.lookup prop $(varE obj))
             then fail $ unpack name ++ " requires property " ++ unpack prop
             else return ()
           Just (Choice2of2 depSchema) -> case validate $(varE $ mkName "graph") depSchema (Object $(varE obj)) of
             Nothing -> return ()
             Just err -> fail err
      |]
    checkAdditionalProperties _ (Choice1of2 True) = [| return () |]
    checkAdditionalProperties _ (Choice1of2 False) = [| fail "additional properties are not allowed" |]
    checkAdditionalProperties value (Choice2of2 sch) = caseE [| validate $(varE $ mkName "graph") $(lift sch) $(value) |]
      [ match (conP 'Nothing []) (normalB [| return () |]) []
      , match (conP 'Just [varP "err"]) (normalB [| fail $(varE "err") |]) []
      ]
    checkPatternAndAdditionalProperties patterns additional = noBindS
      [| let items = HM.toList $(varE obj) in forM_ items $ \(name, value) -> do
           let matchingPatterns = filter (flip PCRE.match (unpack name) . patternCompiled . fst) $(lift patterns)
           forM_ matchingPatterns $ \(_, sch) -> case validate $(varE $ mkName "graph") sch value of
             Nothing -> return ()
             Just err -> fail err
           let isAdditionalProperty = null matchingPatterns && not (name `elem` $(lift $ map fst $ HM.toList $ schemaProperties schema))
           when isAdditionalProperty $(checkAdditionalProperties [| value |] additional)
      |]
    additionalPropertiesAllowed (Choice1of2 True) = True
    additionalPropertiesAllowed _ = False
    checkers = catMaybes
      [ if HM.null (schemaDependencies schema) then Nothing else Just (checkDependencies $ schemaDependencies schema)
      , if null (schemaPatternProperties schema) && additionalPropertiesAllowed (schemaAdditionalProperties schema)
        then Nothing
        else Just (checkPatternAndAdditionalProperties (schemaPatternProperties schema) (schemaAdditionalProperties schema))
      ]

generateArray :: Text -> Schema V3 Text -> CodeGenM (TypeQ, ExpQ)
generateArray name schema = case schemaItems schema of
  Nothing -> monomorphicArray (conT ''Value) (varE 'parseJSON)
  Just (Choice1of2 itemsSchema) -> do
    (itemType, itemCode) <- generateSchema (name <> "Item") itemsSchema
    monomorphicArray itemType itemCode
  Just (Choice2of2 itemSchemas) -> do
    let names = map (\i -> name <> "Item" <> pack (show i)) ([0..] :: [Int])
    items <- sequence $ zipWith generateSchema names itemSchemas
    additionalItems <- case schemaAdditionalItems schema of
      Choice1of2 b -> return $ Choice2of3 b
      Choice2of2 sch -> Choice3of3 <$> generateSchema (name <> "AdditionalItems") sch
    tupleArray items additionalItems
  where
    tupleArray :: [(TypeQ, ExpQ)] -> Choice3 Text Bool (TypeQ, ExpQ) -> CodeGenM (TypeQ, ExpQ)
    tupleArray items additionalItems = return (typeWithAdditional, code $ additionalCheckers ++ [noBindS parserWithAdditional])
      where
        items' = flip map (zip [0..] items) $ \(i, (itemType, itemParser)) ->
          let simpleParser = [| $(itemParser) (V.unsafeIndex $(varE arr) i) |]
          in if i < schemaMinItems schema
             then (itemType, simpleParser)
             else (conT ''Maybe `appT` itemType, [| if V.length $(varE arr) > i then Just <$> $(simpleParser) else return Nothing|])
        (itemTypes, itemParsers) = unzip items'
        tupleType = foldl appT (tupleT tupleLen) itemTypes
        tupleParser = foldl (\parser itemParser -> [| $(parser) <*> $(itemParser) |]) [| pure $(conE $ tupleDataName tupleLen) |] itemParsers
        (tupleLen, additionalCheckers, typeWithAdditional, parserWithAdditional) = case additionalItems of
          Choice1of3 _ -> error "not implemented"
          Choice2of3 b -> if b then (length items', [], tupleType, tupleParser)
                               else (length items', [assertStmt [| V.length $(varE arr) <= $(lift $ length items') |] "no additional items allowed"], tupleType, tupleParser)
          Choice3of3 (additionalType, additionalParser) ->
            (length items' + 1, [], tupleType `appT` (listT `appT` additionalType), [| $(tupleParser) <*> mapM $(additionalParser) (V.toList $ V.drop $(lift $ length items') $(varE arr)) |])

    monomorphicArray :: TypeQ -> ExpQ -> CodeGenM (TypeQ, ExpQ)
    monomorphicArray itemType itemCode = return (listT `appT` itemType, code [noBindS [| mapM $(itemCode) (V.toList $(varE arr)) |]])

    arr = mkName "arr"
    code parser = lambdaPattern (conP ''Array [varP arr])
                                (doE $ checkers ++ parser)
                                [| fail "not an array" |]
    checkMinItems m = assertStmt [| V.length $(varE arr) >= m |] $ "array must have at least " ++ show m ++ " items"
    checkMaxItems m = assertStmt [| V.length $(varE arr) <= m |] $ "array must have at most " ++ show m ++ " items"
    checkUnique = assertStmt [| vectorUnique $(varE arr) |] "array items must be unique"
    checkers = catMaybes
      [ if schemaMinItems schema > 0 then Just (checkMinItems $ schemaMinItems schema) else Nothing
      , checkMaxItems <$> schemaMaxItems schema
      , if schemaUniqueItems schema then Just checkUnique else Nothing
      ]

generateAny :: Schema V3 Text -> CodeGenM (TypeQ, ExpQ)
generateAny schema = return (conT ''Value, code)
  where
    val = mkName "val"
    code = lamE [varP val] $ caseE [| validate $(varE $ mkName "graph") $(lift schema) $(varE val) |]
      [ match (conP 'Nothing []) (normalB [| return $(varE val) |]) []
      , match (conP 'Just [varP "err"]) (normalB [| fail $(varE "err") |]) []
      ]