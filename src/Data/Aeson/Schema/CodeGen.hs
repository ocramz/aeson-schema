{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE CPP                        #-}

module Data.Aeson.Schema.CodeGen
  ( Declaration (..)
  , Code
  , generate
  , generateTH
  , generateModule
  ) where

import           Control.Applicative         ((<|>))
import           Control.Arrow               (first, second)
import           Control.Monad               (forM_, unless, when, zipWithM)
import           Control.Monad.RWS.Lazy      (MonadReader (..),
                                              MonadWriter (..), evalRWST)
import           Data.Aeson hiding (Options)
import           Data.Aeson.Types            (parse)
import           Data.Char                   (isAlphaNum, isLetter, toLower,
                                              toUpper)
import qualified Data.HashMap.Lazy           as HM
import qualified Data.HashSet                as HS
import           Data.List                   (mapAccumL, sort, unzip5)
import qualified Data.Map                    as M
import           Data.Maybe                  (catMaybes, isNothing, maybeToList)
import           Data.Monoid                 ((<>))
import           Data.Scientific             (Scientific, floatingOrInteger,
                                              isInteger)
import           Data.Text                   (Text, pack, unpack)
import qualified Data.Text                   as T
import           Data.Traversable            (forM)
import           Data.Tuple                  (swap)
import qualified Data.Vector                 as V
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Text.Regex.PCRE             as PCRE
import           Text.Regex.PCRE.String      (Regex)

import           Data.Aeson.Schema.Choice
import           Data.Aeson.Schema.CodeGenM
import           Data.Aeson.Schema.Helpers
import           Data.Aeson.Schema.Types
import           Data.Aeson.Schema.Validator
import           Data.Aeson.TH.Lift          ()

type SchemaTypes = M.Map Text Name

instance (Lift k, Lift v) => Lift (M.Map k v) where
  lift m = [| M.fromList $(lift $ M.toList m) |]

-- | Extracts all TH declarations
getDecs :: Code -> [Dec]
getDecs code = [ dec | Declaration dec _ <- code ]

-- | Generate data-types and FromJSON instances for all schemas
generateTH :: Graph Schema Text -- ^ Set of schemas
           -> Options
           -> Q ([Dec], M.Map Text Name) -- ^ Generated code and mapping from schema identifiers to type names
generateTH g = fmap (first getDecs) . generate g

-- | Generated a self-contained module that parses and validates values of
-- a set of given schemas.
generateModule :: Text -- ^ Name of the generated module
               -> Graph Schema Text -- ^ Set of schemas
               -> Options
               -> Q (Text, M.Map Text Name) -- ^ Module code and mapping from schema identifiers to type names
generateModule modName g opts = fmap (first $ renderCode . map rewrite) $ generate g opts
  where
    renderCode :: Code -> Text
    renderCode code = T.intercalate "\n\n" $ [langExts <> ghcOpts, modDec, T.intercalate "\n" imprts] ++ map renderDeclaration code
      where
        mods = sort $ _extraModules opts ++ getUsedModules (getDecs code)
        imprts = map (\m -> "import " <> pack m) mods
        modDec = "module " <> modName <> " where"
        -- TH has no support for file-header pragmas so we splice the text in here
        mkHeaderPragmas t = T.intercalate "\n" . map (\s -> T.unwords ["{-#", t, s, "#-}"])
        langExts = mkHeaderPragmas "LANGUAGE" $ _languageExtensions opts
        ghcOpts = mkHeaderPragmas "OPTIONS_GHC" $ _ghcOptsPragmas opts
    rewrite :: Declaration -> Declaration
    rewrite (Declaration dec text) = Declaration (replaceHiddenModules (cleanPatterns dec) (_replaceModules opts)) text
    rewrite a = a

-- | Generate a generalized representation of the code in a Haskell module
generate :: Graph Schema Text -> Options -> Q (Code, M.Map Text Name)
generate graph opts = swap <$> evalRWST (unCodeGenM $ generateTopLevel graph >> return typeMap) (opts, typeMap) used
  where
    (used, typeMap) = second M.fromList $ mapAccumL nameAccum HS.empty (M.keys graph)
    nameAccum usedNames schemaName = second (schemaName,) $ swap $ codeGenNewName (firstUpper $ unpack schemaName) usedNames

generateTopLevel :: Graph Schema Text -> CodeGenM SchemaTypes ()
generateTopLevel graph = do
  (opts, typeMap) <- ask
  graphN <- qNewName "graph"
  when (nameBase graphN /= "graph") $ fail "name graph is already taken"
  graphDecType <- runQ $ sigD graphN [t| Graph Schema Text |]
  graphDec <- runQ $ valD (varP graphN) (normalB $ lift graph) []
  tell [Declaration graphDecType Nothing, Declaration graphDec Nothing]
  forM_ (M.toList graph) $ \(name, schema) -> do
    let typeName = typeMap M.! name
    ((typeQ, fromJsonQ, toJsonQ), defNewtype) <- generateSchema (Just typeName) name schema
    when defNewtype $ do
#if MIN_VERSION_template_haskell(2,12,0)
      let newtypeCon = normalC typeName [bangType (pure $ Bang NoSourceUnpackedness NoSourceStrictness) typeQ]
      newtypeDec <- runQ $ newtypeD (cxt []) typeName [] Nothing newtypeCon [derivClause Nothing . fmap conT $ _derivingTypeclasses opts]
#elif MIN_VERSION_template_haskell(2,11,0)
      let newtypeCon = normalC typeName [bangType (pure $ Bang NoSourceUnpackedness NoSourceStrictness) typeQ]
      newtypeDec <- runQ $ newtypeD (cxt []) typeName [] Nothing newtypeCon (mapM conT $ _derivingTypeclasses opts)
#else
      let newtypeCon = normalC typeName [strictType notStrict typeQ]
      newtypeDec <- runQ $ newtypeD (cxt []) typeName [] newtypeCon (_derivingTypeclasses opts)
#endif

      fromJSONInst <- runQ $ instanceD (cxt []) (conT ''FromJSON `appT` conT typeName)
        [ valD (varP $ mkName "parseJSON") (normalB [| fmap $(conE typeName) . $fromJsonQ |]) []
        ]
      toJSONInst <- runQ $ instanceD (cxt []) (conT ''ToJSON `appT` conT typeName)
        [ funD (mkName "toJSON")
          [ clause [conP typeName [varP $ mkName "val"]] (normalB $ toJsonQ `appE` varE (mkName "val")) []
          ]
        ]
      tell
        [ Declaration newtypeDec Nothing
        , Declaration fromJSONInst Nothing
        , Declaration toJSONInst Nothing
        ]

generateSchema :: Maybe Name -- ^ Name to be used by type declarations
               -> Text -- ^ Describes the position in the schema
               -> Schema Text
               -> CodeGenM SchemaTypes ((TypeQ, ExpQ, ExpQ), Bool) -- ^ ((type of the generated representation (a), function :: Value -> Parser a), whether a newtype wrapper is necessary)
generateSchema decName name schema = case schemaDRef schema of
  Just ref -> askEnv >>= \typesMap -> case M.lookup ref typesMap of
    Nothing -> fail "couldn't find referenced schema"
    Just referencedSchema -> return ((conT referencedSchema, [| parseJSON |], [| toJSON |]), True)
  Nothing -> first (\(typ,from,to) -> (typ,wrap from,to)) <$> case schemaType schema of
    [] -> fail "empty type"
    [Choice1of2 typ] -> generateSimpleType decName name typ
    [Choice2of2 sch] -> generateSchema decName name sch
    unionType -> do
      let l = pack . show $ length unionType
      let names = map (\i -> name <> "Choice" <> pack (show i) <> "of" <> l) ([1..] :: [Int])
      subs <- fmap (map fst) $ zipWithM (choice2 (flip $ generateSimpleType Nothing) (flip $ generateSchema Nothing)) unionType names
      (,True) <$> generateUnionType subs
  where
    generateSimpleType :: Maybe Name -> Text -> SchemaType -> CodeGenM SchemaTypes ((TypeQ, ExpQ, ExpQ), Bool)
    generateSimpleType decName' name' typ = case typ of
      StringType  -> (,True) <$> generateString schema
      NumberType  -> (,True) <$> generateNumber schema
      IntegerType -> (,True) <$> generateInteger schema
      BooleanType -> (,True) <$> generateBoolean
      ObjectType  -> case checkers of
        [] -> generateObject decName' name' schema
        _  -> (,True) . fst <$> generateObject Nothing name' schema
      ArrayType   -> (,True) <$> generateArray name' schema
      NullType    -> (,True) <$> generateNull
      AnyType     -> (,True) <$> generateAny schema
    generateUnionType :: [(TypeQ, ExpQ, ExpQ)] -> CodeGenM SchemaTypes (TypeQ, ExpQ, ExpQ)
    generateUnionType union = return (typ, lamE [varP val] fromQ, toQ)
      where
        n = length union
        (types, froms, tos) = unzip3 union
        unionParsers = zipWith (\i parser -> [| $(choiceConE i n) <$> $parser $(varE val) |]) [1..] froms
        choiceConE :: Int -> Int -> ExpQ
        choiceConE i j = conE $ mkName $ "Data.Aeson.Schema.Choice.Choice" ++ show i ++ "of" ++ show j
        choiceT i = conT $ mkName $ "Data.Aeson.Schema.Choice.Choice" ++ show i
        typ = foldl appT (choiceT n) types
        fromQ = foldr (\choiceParser unionParser -> [| $choiceParser <|> $unionParser |]) [| fail "no type in union" |] unionParsers
        toQ = foldl appE (varE $ mkName $ "Data.Aeson.Schema.Choice.choice" ++ show n) tos
    val = mkName "val"
    checkEnum xs = assertStmt [| $(varE val) `elem` xs |] "not one of the values in enum"
    checkDisallow dis = noBindS $ doE $ map (noBindS . choice2 disallowType disallowSchema) dis
    disallowType StringType  = disallowPattern (conP 'String [wildP]) "strings are disallowed"
    disallowType NumberType  = disallowPattern (conP 'Number [wildP]) "numbers are disallowed"
    disallowType IntegerType =
      [| case $(varE val) of
           Number num | isInteger num -> fail "integers are disallowed"
           _ -> return ()
      |]
    disallowType BooleanType = disallowPattern (conP 'Bool [wildP]) "booleans are disallowed"
    disallowType ObjectType  = disallowPattern (conP 'Object [wildP]) "objects are disallowed"
    disallowType ArrayType   = disallowPattern (conP 'Array [wildP]) "arrays are disallowed"
    disallowType NullType    = disallowPattern (conP 'Null []) "null is disallowed"
    disallowType AnyType     = [| fail "Nothing is allowed here. Sorry." |]
    disallowPattern :: PatQ -> String -> ExpQ
    disallowPattern pat err = caseE (varE val)
      [ match pat (normalB [| fail err |])[]
      , match wildP (normalB [| return () |]) []
      ]
    disallowSchema sch =
      [| case validate $(varE $ mkName "graph") $(lift sch) $(varE val) of
           [] -> fail "disallowed"
           _  -> return ()
      |]
    checkExtends exts = noBindS $ doE $ flip map exts $ flip assertValidates (varE val) . lift
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

assertValidates :: ExpQ -> ExpQ -> StmtQ
assertValidates schema value = noBindS $ parensE
  [| case validate $(varE $ mkName "graph") $schema $value of
       [] -> return ()
       es -> fail $ unlines es
  |]

lambdaPattern :: PatQ -> ExpQ -> ExpQ -> ExpQ
lambdaPattern pat body err = lamE [varP val] $ caseE (varE val)
  [ match pat (normalB body) []
  , match wildP (normalB err) []
  ]
  where val = mkName "val"

generateString :: Schema Text -> CodeGenM SchemaTypes (TypeQ, ExpQ, ExpQ)
generateString schema = return (conT ''Text, code, [| String |])
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

generateNumber :: Schema Text -> CodeGenM SchemaTypes (TypeQ, ExpQ, ExpQ)
generateNumber schema = return (conT ''Scientific, code, [| Number |])
  where
    num = mkName "num"
    code = lambdaPattern (conP 'Number [varP num])
                         (doE $ numberCheckers num schema ++ [noBindS [| return $(varE num) |]])
                         [| fail "not a number" |]

generateInteger :: Schema Text -> CodeGenM SchemaTypes (TypeQ, ExpQ, ExpQ)
generateInteger schema = return (conT ''Integer, code, [| Number . fromInteger |])
  where
    num = mkName "num"
    code = lambdaPattern (conP 'Number [varP num])
                         [| case floatingOrInteger $(varE num) of
                              Right i -> $(doE $ numberCheckers num schema ++
                                                 [noBindS [| return i |]])
                              _ -> fail "not an integer"
                         |]
                         [| fail "not an integer" |]

numberCheckers :: Name -> Schema Text -> [StmtQ]
numberCheckers num schema = catMaybes
  [ checkMinimum (schemaExclusiveMinimum schema) <$> schemaMinimum schema
  , checkMaximum (schemaExclusiveMaximum schema) <$> schemaMaximum schema
  , checkDivisibleBy <$> schemaDivisibleBy schema
  ]
  where
    checkMinimum, checkMaximum :: Bool -> Scientific -> StmtQ
    checkMinimum excl m = if excl
      then assertStmt [| $(varE num) >  m |] $ "number must be greater than " ++ show m
      else assertStmt [| $(varE num) >= m |] $ "number must be greater than or equal " ++ show m
    checkMaximum excl m = if excl
      then assertStmt [| $(varE num) <  m |] $ "number must be less than " ++ show m
      else assertStmt [| $(varE num) <= m |] $ "number must be less than or equal " ++ show m
    checkDivisibleBy devisor = assertStmt [| $(varE num) `isDivisibleBy` devisor |] $ "number must be devisible by " ++ show devisor

generateBoolean :: CodeGenM SchemaTypes (TypeQ, ExpQ, ExpQ)
generateBoolean = return ([t| Bool |], [| parseJSON |], [| Bool |])

generateNull :: CodeGenM SchemaTypes (TypeQ, ExpQ, ExpQ)
generateNull = return (tupleT 0, code, [| const Null |])
  where
    code = lambdaPattern (conP 'Null [])
                         [| return () |]
                         [| fail "not null" |]

cleanName :: String -> String
cleanName str = charFirst
  where
    isAllowed c = isAlphaNum c || c `elem` ("'_"::String)
    cleaned = filter isAllowed str
    charFirst = case cleaned of
      (chr:_) | not (isLetter chr || chr == '_') -> '_':cleaned
      _ -> cleaned
firstUpper, firstLower :: String -> String
firstUpper "" = ""
firstUpper (c:cs) = toUpper c : cs
firstLower "" = ""
firstLower (c:cs) = toLower c : cs

generateObject :: Maybe Name -- ^ Name to be used by data declaration
               -> Text
               -> Schema Text
               -> CodeGenM SchemaTypes ((TypeQ, ExpQ, ExpQ), Bool)
generateObject decName name schema = case (propertiesList, schemaAdditionalProperties schema) of
  ([], Choice2of2 additionalSchema) -> generateMap additionalSchema
  _                                 -> generateDataDecl
  where
    propertiesList = HM.toList $ schemaProperties schema
    generateMap :: Schema Text -> CodeGenM SchemaTypes ((TypeQ, ExpQ, ExpQ), Bool)
    generateMap additionalSchema = case schemaPatternProperties schema of
      [] -> do
        ((additionalType, additionalParser, additionalTo), _) <-
          generateSchema Nothing (name <> "Item") additionalSchema
        let parseAdditional = [| fmap M.fromList $ mapM (\(k,v) -> (,) k <$> $(additionalParser) v) $ HM.toList $(varE obj) |]
        let parser = lambdaPattern (conP 'Object [varP obj])
                                   (doE $ checkers ++ [noBindS parseAdditional])
                                   [| fail "not an object" |]
        let typ = [t| M.Map Text $(additionalType) |]
        let to = [| Object . HM.fromList . map (second $(additionalTo)) . M.toList |]
        return ((typ, parser, to), True)
      _  -> do
        let validatesStmt = assertValidates (lift schema) [| Object $(varE obj) |]
        let parser = lambdaPattern (conP 'Object [varP obj])
                                   (doE $ validatesStmt : [noBindS [| return $ M.fromList $ HM.toList $(varE obj) |]])
                                   [| fail "not an object" |]
        return (([t| M.Map Text Value |], parser, [| Object . HM.fromList . M.toList |]), True)
    generateDataDecl :: CodeGenM SchemaTypes ((TypeQ, ExpQ, ExpQ), Bool)
    generateDataDecl = do
      (propertyNames, propertyTypes, propertyParsers, propertyTos, defaultParsers) <- fmap unzip5 $ forM propertiesList $ \(fieldName, propertySchema) -> do
        let cleanedFieldName = cleanName $ unpack name ++ firstUpper (unpack fieldName)
        propertyName <- qNewName $ firstLower cleanedFieldName
        ((typ, fromExpr, toExpr), _) <-
          generateSchema Nothing (pack (firstUpper cleanedFieldName)) propertySchema
        let lookupProperty = [| HM.lookup $(lift fieldName) $(varE obj) |]
        case schemaDefault propertySchema of
          Just defaultValue -> do
            defaultName <- qNewName $ "default" <> firstUpper cleanedFieldName
            return ( propertyName
                   , typ
                   , [| maybe (return $(varE defaultName)) $fromExpr $lookupProperty |]
                   , [| Just . $toExpr |]
                   , Just $ valD (conP 'Success [varP defaultName]) (normalB [| parse $fromExpr $(lift defaultValue) |]) []
                   )
          Nothing -> return $ if schemaRequired propertySchema
            then ( propertyName
                 , typ
                 , [| maybe (fail $(lift $ "required property " ++ unpack fieldName ++ " missing")) $fromExpr $lookupProperty |]
                 , [| Just . $toExpr |]
                 , Nothing
                 )
            else ( propertyName
                 , conT ''Maybe `appT` typ
                 , [| traverse $fromExpr $lookupProperty |]
                 , [| fmap $toExpr |]
                 , Nothing
                 )
      conName <- maybe (qNewName $ firstUpper $ unpack name) return decName
      tcs <- _derivingTypeclasses <$> askOpts
      rMods <- _replaceModules <$> askOpts
      userInstanceGen <- _extraInstances <$> askOpts
      recordDeclaration <- runQ $ genRecord conName
                                            (zip3 propertyNames
                                                  (map (fmap (`replaceHiddenModules` rMods)) propertyTypes)
                                                  (map (schemaDescription . snd) propertiesList))
                                            (map (`replaceHiddenModules` rMods) tcs)
      let typ = conT conName
      let parser = foldl (\oparser propertyParser -> [| $oparser <*> $propertyParser |]) [| pure $(conE conName) |] propertyParsers
      fromJSONInst <- runQ $ instanceD (cxt []) (conT ''FromJSON `appT` typ)
        [ funD (mkName "parseJSON") -- cannot use a qualified name here
            [ clause [conP 'Object [varP obj]] (normalB $ doE $ checkers ++ [noBindS parser]) (catMaybes defaultParsers)
            , clause [wildP] (normalB [| fail "not an object" |]) []
            ]
        ]
      let paramNames = map (mkName . ("a" ++) . show) $ take (length propertyTos) ([1..] :: [Int])

      userInstances <- runQ . sequence $ userInstanceGen conName
      toJSONInst <- runQ $ instanceD (cxt []) (conT ''ToJSON `appT` typ)
        [ funD (mkName "toJSON") -- cannot use a qualified name here
          [ clause [conP conName $ map varP paramNames] (normalB [| Object $ HM.fromList $ catMaybes $(listE $ zipWith3 (\fieldName to param -> [| (,) $(lift fieldName) <$> $to $(varE param) |]) (map fst propertiesList) propertyTos paramNames) |]) []
          ]
        ]
      tell $
        [ recordDeclaration ]
        ++ map (flip Declaration Nothing) userInstances ++
        [ Declaration fromJSONInst Nothing
        , Declaration toJSONInst Nothing
        ]
      return ((typ, [| parseJSON |], [| toJSON |]), False)
    obj = mkName "obj"
    checkDependencies deps = noBindS
      [| let items = HM.toList $(varE obj) in forM_ items $ \(pname, _) -> case HM.lookup pname $(lift deps) of
           Nothing -> return ()
           Just (Choice1of2 props) -> forM_ props $ \prop -> when (isNothing (HM.lookup prop $(varE obj))) $
             fail $ unpack pname ++ " requires property " ++ unpack prop
           Just (Choice2of2 depSchema) -> $(doE [assertValidates [| depSchema |] [| Object $(varE obj) |]])
      |]
    checkAdditionalProperties _ (Choice1of2 True) = [| return () |]
    checkAdditionalProperties _ (Choice1of2 False) = [| fail "additional properties are not allowed" |]
    checkAdditionalProperties value (Choice2of2 sch) = doE [assertValidates (lift sch) value]
    -- TODO Once https://ghc.haskell.org/trac/ghc/ticket/10734 is
    -- fixed, use a ‘let’ again for matchingPatterns and
    -- isAdditionalProperty
    checkPatternAndAdditionalProperties patterns additional = noBindS
      [| let items = HM.toList $(varE obj) in forM_ items $ \(pname, value) -> do
           matchingPatterns <- return (filter (flip PCRE.match (unpack pname) . patternCompiled . fst) $(lift patterns))
           forM_ matchingPatterns $ \(_, sch) -> $(doE [assertValidates [| sch |] [| value |]])
           isAdditionalProperty <- return (null matchingPatterns && pname `notElem` $(lift $ map fst $ HM.toList $ schemaProperties schema))
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

generateArray :: Text -> Schema Text -> CodeGenM SchemaTypes (TypeQ, ExpQ, ExpQ)
generateArray name schema = case schemaItems schema of
  Nothing -> monomorphicArray (conT ''Value) [| parseJSON |] [| toJSON |]
  Just (Choice1of2 itemsSchema) -> do
    ((itemType, itemParse, itemTo), _) <- generateSchema Nothing (name <> "Item") itemsSchema
    monomorphicArray itemType itemParse itemTo
  Just (Choice2of2 itemSchemas) -> do
    let names = map (\i -> name <> "Item" <> pack (show i)) ([0..] :: [Int])
    items <- fmap (map fst) $ zipWithM (generateSchema Nothing) names itemSchemas
    additionalItems <- case schemaAdditionalItems schema of
      Choice1of2 b -> return $ Choice1of2 b
      Choice2of2 sch -> Choice2of2 . fst <$> generateSchema Nothing (name <> "AdditionalItems") sch
    tupleArray items additionalItems
  where
    tupleArray :: [(TypeQ, ExpQ, ExpQ)]
               -> Choice2 Bool (TypeQ, ExpQ, ExpQ)
               -> CodeGenM SchemaTypes (TypeQ, ExpQ, ExpQ)
    tupleArray items additionalItems = return (tupleType, code $ additionalCheckers ++ [noBindS tupleParser], tupleTo)
      where
        items' = flip map (zip [0::Int ..] items) $ \(i, (itemType, itemParser, itemTo)) ->
          let simpleParser = [| $(itemParser) (V.unsafeIndex $(varE arr) i) |]
          in if i < schemaMinItems schema
             then (itemType, simpleParser, [| return . $itemTo |])
             else ( conT ''Maybe `appT` itemType
                  , [| if V.length $(varE arr) > i then Just <$> $(simpleParser) else return Nothing|]
                  , [| maybeToList . fmap $itemTo |]
                  )
        (additionalCheckers, maybeAdditionalTypeAndParser) = case additionalItems of
          Choice1of2 b -> if b
            then ([], Nothing)
            else ([assertStmt [| V.length $(varE arr) <= $(lift $ length items') |] "no additional items allowed"], Nothing)
          Choice2of2 (additionalType, additionalParser, additionalTo) ->
            ( []
            , Just ( listT `appT` additionalType
                   , [| mapM $(additionalParser) (V.toList $ V.drop $(lift $ length items') $(varE arr)) |]
                   , [| map $additionalTo |]
                   )
            )
        items'' = items' ++ maybeToList maybeAdditionalTypeAndParser
        (_itemTypes, _itemParsers, itemTos) = unzip3 items''
        (tupleType, tupleParser, tupleTo) = case items'' of
          [(itemType, itemParser, itemTo)] -> (itemType, itemParser, [| Array . V.fromList . $itemTo |])
          _ -> let tupleFields = map (mkName . ("f" ++) . show) $ take (length items'') ([1..] :: [Int])
                   (a, b) = foldl (\(typ, parser) (itemType, itemParser, _) -> (typ `appT` itemType, [| $(parser) <*> $(itemParser) |]))
                     (tupleT $ length items'', [| pure $(conE $ tupleDataName $ length items'') |])
                     items''
                   to = lamE [tupP $ map varP tupleFields]
                             [| Array $ V.fromList $ concat $(listE $ zipWith appE itemTos (map varE tupleFields)) |]
               in (a, b, to)

    monomorphicArray :: TypeQ -> ExpQ -> ExpQ -> CodeGenM SchemaTypes (TypeQ, ExpQ, ExpQ)
    monomorphicArray itemType itemParse itemTo = return
      ( listT `appT` itemType
      , code [noBindS [| mapM $(itemParse) (V.toList $(varE arr)) |]]
      , [| Array . V.fromList . map $itemTo |]
      )

    arr = mkName "arr"
    code parser = lambdaPattern (conP 'Array [varP arr])
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

generateAny :: Schema Text -> CodeGenM SchemaTypes (TypeQ, ExpQ, ExpQ)
generateAny schema = return (conT ''Value, code, [| id |])
  where
    val = mkName "val"
    code = lamE [varP val]
                (doE [ assertValidates (lift schema) (varE val)
                     , noBindS [| return $(varE val) |]
                     ])
