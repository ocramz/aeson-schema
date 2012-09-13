{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Data.Aeson.Schema.CodeGen
  ( Declaration (..)
  , Code
  , generate
  , generateTH
  , generateModule
  ) where

import           Control.Applicative         (Applicative (..), (<$>), (<*>),
                                              (<|>))
import           Control.Arrow               (first, second)
import           Control.Monad               (forM_, unless, when)
import           Control.Monad.RWS.Lazy      (MonadReader (..), MonadState (..),
                                              MonadWriter (..), RWST (..),
                                              evalRWST)
import qualified Control.Monad.Trans.Class   as MT
import           Data.Aeson
import           Data.Aeson.Types            (parse)
import           Data.Attoparsec.Number      (Number (..))
import           Data.Char                   (isAlphaNum, isLetter, toLower,
                                              toUpper)
import           Data.Function               (on)
import qualified Data.HashMap.Lazy           as HM
import qualified Data.HashSet                as HS
import           Data.List                   (mapAccumL, sort, unzip4)
import qualified Data.Map                    as M
import           Data.Maybe                  (catMaybes, isNothing, maybeToList)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text, pack, unpack)
import qualified Data.Text                   as T
import           Data.Traversable            (forM, traverse)
import           Data.Tuple                  (swap)
import qualified Data.Vector                 as V
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Text.Regex.PCRE             as PCRE
import           Text.Regex.PCRE.String      (Regex)

import           Data.Aeson.Schema.Types
import           Data.Aeson.Schema.Choice
import           Data.Aeson.Schema.Helpers
import           Data.Aeson.Schema.Validator
import           Data.Aeson.TH.Lift ()


data Declaration = Declaration Dec (Maybe Text)
                 | Comment Text
                 deriving (Show, Eq)
type Code = [Declaration]

type StringSet = HS.HashSet String
type SchemaTypes = M.Map Text Name

newtype CodeGenM a = CodeGenM
  { unCodeGenM :: RWST SchemaTypes Code StringSet Q a
  } deriving (Monad, Applicative, Functor, MonadReader SchemaTypes, MonadWriter Code, MonadState StringSet)

codeGenNewName :: String -> StringSet -> (Name, StringSet)
codeGenNewName s used = (Name (mkOccName free) NameS, HS.insert free used)
  where
    free = head $ dropWhile (`HS.member` used) $ (if validName s then (s:) else id) $ map (\i -> s ++ "_" ++ show i) ([1..] :: [Int])
    -- taken from http://www.haskell.org/haskellwiki/Keywords
    haskellKeywords = HS.fromList
      [ "as", "case", "of", "class", "data", "data family", "data instance"
      , "default", "deriving", "deriving instance", "do", "forall", "foreign"
      , "hiding", "if", "then", "else", "import", "infix", "infixl", "infixr"
      , "instance", "let", "in", "mdo", "module", "newtype", "proc"
      , "qualified", "rec", "type", "type family", "type instance", "where"
      ]
    validName n = not (n `elem` ["", "_"] || n `HS.member` haskellKeywords)

instance Quasi CodeGenM where
  qNewName = state . codeGenNewName
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

instance (Lift k, Lift v) => Lift (M.Map k v) where
  lift m = [| M.fromList $(lift $ M.toList m) |]

extraModules :: [String]
extraModules =
  [ "Text.Regex" -- provides RegexMaker instances
  , "Text.Regex.PCRE.String" -- provides RegexLike instances, Regex type
  , "Data.Aeson.Types" -- Parser type
  , "Data.Ratio"
  ]

getDecs :: Code -> [Dec]
getDecs = catMaybes . map getDec
  where getDec (Declaration dec _) = Just dec
        getDec _ = Nothing

generateTH :: Graph Schema Text -> Q ([Dec], M.Map Text Name)
generateTH = fmap (first getDecs) . generate

generateModule :: Text -> Graph Schema Text -> Q (Text, M.Map Text Name)
generateModule modName = fmap (first $ renderCode . map rewrite) . generate
  where
    renderCode :: Code -> Text
    renderCode code = T.unlines $ [modDec] ++ imprts ++ map renderDeclaration code
      where
        mods = sort $ extraModules ++ getUsedModules (getDecs code)
        imprts = map (\m -> "import " <> pack m) mods
        modDec = "module " <> modName <> " where"
    rewrite :: Declaration -> Declaration
    rewrite (Declaration dec text) = Declaration (replaceHiddenModules dec) text
    rewrite a = a
    renderDeclaration :: Declaration -> Text
    renderDeclaration (Declaration _ (Just text)) = text
    renderDeclaration (Declaration dec Nothing)   = pack (pprint dec)
    renderDeclaration (Comment comment)           = T.unlines $ map (\line -> "-- " <> line) $ T.lines comment

generate :: Graph Schema Text -> Q (Code, M.Map Text Name)
generate graph = swap <$> evalRWST (unCodeGenM $ generateTopLevel graph >> return typeMap) typeMap used
  where
    (used, typeMap) = second M.fromList $ mapAccumL nameAccum HS.empty (M.keys graph)
    nameAccum usedNames schemaName = second (schemaName,) $ swap $ codeGenNewName (firstUpper $ unpack schemaName) usedNames

generateTopLevel :: Graph Schema Text -> CodeGenM ()
generateTopLevel graph = do
  typeMap <- ask
  graphN <- qNewName "graph"
  when (nameBase graphN /= "graph") $ fail "name graph is already taken"
  graphDecType <- runQ $ sigD graphN [t| Graph Schema Text |]
  graphDec <- runQ $ valD (varP graphN) (normalB $ lift graph) []
  tell [Declaration graphDecType Nothing, Declaration graphDec Nothing]
  forM_ (M.toList graph) $ \(name, schema) -> do
    let typeName = typeMap M.! name
    ((typeQ, exprQ), defNewtype) <- generateSchema (Just typeName) name schema
    when defNewtype $ do
      let newtypeCon = normalC typeName [strictType notStrict typeQ]
      newtypeDec <- runQ $ newtypeD (cxt []) typeName [] newtypeCon []
      fromJSONInst <- runQ $ instanceD (cxt []) (conT ''FromJSON `appT` conT typeName)
        [ valD (varP $ mkName "parseJSON") (normalB [| fmap $(conE typeName) . $exprQ |]) []
        ]
      tell [Declaration newtypeDec Nothing, Declaration fromJSONInst Nothing]

generateSchema :: Maybe Name -> Text -> Schema Text -> CodeGenM ((TypeQ, ExpQ), Bool)
generateSchema decName name schema = case schemaDRef schema of
  Just ref -> ask >>= \typesMap -> case M.lookup ref typesMap of
    Nothing -> fail "couldn't find referenced schema"
    Just referencedSchema -> return ((conT referencedSchema, [| parseJSON |]), True)
  Nothing -> first (second wrap) <$> case schemaType schema of
    [] -> fail "empty type"
    [Choice1of2 typ] -> generateSimpleType decName name typ
    [Choice2of2 sch] -> generateSchema decName name sch
    unionType -> do
      let l = pack . show $ length unionType
      let names = map (\i -> name <> "Choice" <> pack (show i) <> "of" <> l) ([1..] :: [Int])
      subs <- fmap (map fst) $ sequence $ zipWith (choice2 (flip $ generateSimpleType Nothing) (flip $ generateSchema Nothing)) unionType names
      (,True) <$> generateUnionType subs
  where
    generateSimpleType :: Maybe Name -> Text -> SchemaType -> CodeGenM ((TypeQ, ExpQ), Bool)
    generateSimpleType decName' name' typ = case typ of
      StringType  -> (,True) <$> generateString schema
      NumberType  -> (,True) <$> generateNumber schema
      IntegerType -> (,True) <$> generateInteger schema
      BooleanType -> (,True) <$> generateBoolean
      ObjectType  -> case checkers of
        [] -> (,False) <$> generateObject decName' name' schema
        _  -> (,True)  <$> generateObject Nothing name' schema
      ArrayType   -> (,True) <$> generateArray name' schema
      NullType    -> (,True) <$> generateNull
      AnyType     -> (,True) <$> generateAny schema
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
      [ match (conP 'Just [varP $ mkName "err"]) (normalB [| fail $(varE $ mkName "err") |]) []
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
assertStmt expr err = noBindS [| unless $(expr) (fail err) |]

lambdaPattern :: PatQ -> ExpQ -> ExpQ -> ExpQ
lambdaPattern pat body err = lamE [varP val] $ caseE (varE val)
  [ match pat (normalB body) []
  , match wildP (normalB err) []
  ]
  where val = mkName "val"

generateString :: Schema Text -> CodeGenM (TypeQ, ExpQ)
generateString schema = return (conT ''Text, code)
  where
    str = mkName "str"
    checkMinLength l = assertStmt [| T.length $(varE str) >= l |] $ "string must have at least " ++ show l ++ " characters"
    checkMaxLength l = assertStmt [| T.length $(varE str) <= l |] $ "string must have at most " ++ show l ++ " characters"
    checkPattern (Pattern p _) = noBindS $ doE
      [ bindS (varP $ mkName "regex") [| PCRE.makeRegexM $(lift (T.unpack p)) |]
      , assertStmt [| PCRE.match ($(varE $ mkName "regex") :: Regex) (unpack $(varE str)) |] $ "string must match pattern " ++ show p
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

generateNumber :: Schema Text -> CodeGenM (TypeQ, ExpQ)
generateNumber schema = return (conT ''Number, code)
  where
    num = mkName "num"
    code = lambdaPattern (conP 'Number [varP num])
                         (doE $ numberCheckers num schema ++ [noBindS [| return $(varE num) |]])
                         [| fail "not a number" |]

generateInteger :: Schema Text -> CodeGenM (TypeQ, ExpQ)
generateInteger schema = return (conT ''Integer, code)
  where
    num = mkName "num"
    code = lambdaPattern (conP 'Number [asP num $ conP 'I [varP $ mkName "i"]])
                         (doE $ numberCheckers num schema ++ [noBindS [| return $(varE $ mkName "i") |]])
                         [| fail "not an integer" |]

numberCheckers :: Name -> Schema Text -> [StmtQ]
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
      (chr:_) | not (isLetter chr || chr == '_') -> '_':cleaned
      _ -> cleaned
firstUpper, firstLower :: String -> String
firstUpper "" = ""
firstUpper (c:cs) = toUpper c : cs
firstLower "" = ""
firstLower (c:cs) = toLower c : cs

generateObject :: Maybe Name -> Text -> Schema Text -> CodeGenM (TypeQ, ExpQ)
generateObject decName name schema = do
  let propertiesList = HM.toList $ schemaProperties schema
  (propertyNames, propertyTypes, propertyParsers, defaultParsers) <- fmap unzip4 $ forM propertiesList $ \(fieldName, propertySchema) -> do
    let cleanedFieldName = cleanName $ unpack fieldName
    propertyName <- qNewName $ firstLower $ cleanedFieldName
    ((typ, expr), _) <- generateSchema Nothing (name <> pack (firstUpper cleanedFieldName)) propertySchema
    let lookupProperty = [| HM.lookup $(lift fieldName) $(varE obj) |]
    case schemaDefault propertySchema of
      Just defaultValue -> do
        defaultName <- qNewName $ "default" <> firstUpper cleanedFieldName
        return (propertyName, typ, [| maybe (return $(varE defaultName)) $expr $lookupProperty |], Just $ valD (conP 'Success [varP defaultName]) (normalB [| parse $expr $(lift defaultValue) |]) [])
      Nothing -> return $ if schemaRequired propertySchema
        then (propertyName, typ, [| maybe (fail $(lift $ "required property " ++ unpack fieldName ++ " missing")) $expr $lookupProperty |], Nothing)
        else (propertyName, conT ''Maybe `appT` typ, [| traverse $expr $lookupProperty |], Nothing)
  conName <- maybe (qNewName $ firstUpper $ unpack name) return decName
  let typ = conT conName
  let dataCon = recC conName $ zipWith (\pname ptyp -> (pname,NotStrict,) <$> ptyp) propertyNames propertyTypes
  dataDec <- runQ $ dataD (cxt []) conName [] [dataCon] []
  let parser = foldl (\oparser propertyParser -> [| $oparser <*> $propertyParser |]) [| pure $(conE conName) |] propertyParsers
  fromJSONInst <- runQ $ instanceD (cxt []) (conT ''FromJSON `appT` typ)
    [ funD (mkName "parseJSON") -- cannot use a qualified name here
        [ clause [conP 'Object [varP obj]] (normalB $ doE $ checkers ++ [noBindS parser]) (catMaybes defaultParsers)
        , clause [wildP] (normalB [| fail "not an object" |]) []
        ]
    ]
  tell [Declaration dataDec Nothing, Declaration fromJSONInst Nothing]
  return (typ, [| parseJSON |])
  where
    obj = mkName "obj"
    checkDependencies deps = noBindS
      [| let items = HM.toList $(varE obj) in forM_ items $ \(pname, _) -> case HM.lookup pname $(lift deps) of
           Nothing -> return ()
           Just (Choice1of2 props) -> forM_ props $ \prop -> if isNothing (HM.lookup prop $(varE obj))
             then fail $ unpack pname ++ " requires property " ++ unpack prop
             else return ()
           Just (Choice2of2 depSchema) -> case validate $(varE $ mkName "graph") depSchema (Object $(varE obj)) of
             Nothing -> return ()
             Just err -> fail err
      |]
    checkAdditionalProperties _ (Choice1of2 True) = [| return () |]
    checkAdditionalProperties _ (Choice1of2 False) = [| fail "additional properties are not allowed" |]
    checkAdditionalProperties value (Choice2of2 sch) = caseE [| validate $(varE $ mkName "graph") $(lift sch) $(value) |]
      [ match (conP 'Nothing []) (normalB [| return () |]) []
      , match (conP 'Just [varP $ mkName "err"]) (normalB [| fail $(varE $ mkName "err") |]) []
      ]
    checkPatternAndAdditionalProperties patterns additional = noBindS
      [| let items = HM.toList $(varE obj) in forM_ items $ \(pname, value) -> do
           let matchingPatterns = filter (flip PCRE.match (unpack pname) . patternCompiled . fst) $(lift patterns)
           forM_ matchingPatterns $ \(_, sch) -> case validate $(varE $ mkName "graph") sch value of
             Nothing -> return ()
             Just err -> fail err
           let isAdditionalProperty = null matchingPatterns && not (pname `elem` $(lift $ map fst $ HM.toList $ schemaProperties schema))
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

generateArray :: Text -> Schema Text -> CodeGenM (TypeQ, ExpQ)
generateArray name schema = case schemaItems schema of
  Nothing -> monomorphicArray (conT ''Value) (varE 'parseJSON)
  Just (Choice1of2 itemsSchema) -> do
    ((itemType, itemCode), _) <- generateSchema Nothing (name <> "Item") itemsSchema
    monomorphicArray itemType itemCode
  Just (Choice2of2 itemSchemas) -> do
    let names = map (\i -> name <> "Item" <> pack (show i)) ([0..] :: [Int])
    items <- fmap (map fst) $ sequence $ zipWith (generateSchema Nothing) names itemSchemas
    additionalItems <- case schemaAdditionalItems schema of
      Choice1of2 b -> return $ Choice1of2 b
      Choice2of2 sch -> Choice2of2 . fst <$> generateSchema Nothing (name <> "AdditionalItems") sch
    tupleArray items additionalItems
  where
    tupleArray :: [(TypeQ, ExpQ)] -> Choice2 Bool (TypeQ, ExpQ) -> CodeGenM (TypeQ, ExpQ)
    tupleArray items additionalItems = return (tupleType, code $ additionalCheckers ++ [noBindS tupleParser])
      where
        items' = flip map (zip [0..] items) $ \(i, (itemType, itemParser)) ->
          let simpleParser = [| $(itemParser) (V.unsafeIndex $(varE arr) i) |]
          in if i < schemaMinItems schema
             then (itemType, simpleParser)
             else (conT ''Maybe `appT` itemType, [| if V.length $(varE arr) > i then Just <$> $(simpleParser) else return Nothing|])
        (additionalCheckers, maybeAdditionalTypeAndParser) = case additionalItems of
          Choice1of2 b -> if b
            then ([], Nothing)
            else ([assertStmt [| V.length $(varE arr) <= $(lift $ length items') |] "no additional items allowed"], Nothing)
          Choice2of2 (additionalType, additionalParser) ->
            ( []
            , Just (listT `appT` additionalType, [| mapM $(additionalParser) (V.toList $ V.drop $(lift $ length items') $(varE arr)) |])
            )
        items'' = items' ++ maybeToList maybeAdditionalTypeAndParser
        (itemTypes, itemParsers) = unzip items''
        (tupleType, tupleParser) = case items'' of
          [(itemType, itemParser)] -> (itemType, itemParser)
          _ -> foldl (\(typ, parser) (itemType, itemParser) -> (typ `appT` itemType, [| $(parser) <*> $(itemParser) |]))
                     (tupleT $ length items'', [| pure $(conE $ tupleDataName $ length items'') |])
                     items''

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

generateAny :: Schema Text -> CodeGenM (TypeQ, ExpQ)
generateAny schema = return (conT ''Value, code)
  where
    val = mkName "val"
    code = lamE [varP val] $ caseE [| validate $(varE $ mkName "graph") $(lift schema) $(varE val) |]
      [ match (conP 'Nothing []) (normalB [| return $(varE val) |]) []
      , match (conP 'Just [varP $ mkName "err"]) (normalB [| fail $(varE $ mkName "err") |]) []
      ]
