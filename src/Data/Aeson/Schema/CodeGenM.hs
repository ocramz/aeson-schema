{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Data.Aeson.Schema.CodeGenM
  ( Declaration (..)
  , Code
  , CodeGenM (..)
  , renderDeclaration
  , codeGenNewName
  , genRecord
  ) where

import           Control.Applicative        (Applicative (..), (<$>))
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.RWS.Lazy     (MonadReader (..), MonadState (..),
                                             MonadWriter (..), RWST (..))
import qualified Control.Monad.Trans.Class  as MT
import           Data.Data                  (Data, Typeable)
import           Data.Function              (on)
import qualified Data.HashSet               as HS
import           Data.Monoid                ((<>), mconcat)
import           Data.Text                  (Text, pack)
import qualified Data.Text                  as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

-- | A top-level declaration.
data Declaration = Declaration Dec (Maybe Text) -- ^ Optional textual declaration. This can be used for information (e.g. inline comments) that are not representable in TH.
                 | Comment Text -- ^ Comment text
                 deriving (Show, Eq, Typeable, Data)

-- | Render a declaration. When a declaration contains both a TH syntax tree and a text representation, the text representation is preferred.
renderDeclaration :: Declaration -> Text
renderDeclaration (Declaration _ (Just text)) = text
renderDeclaration (Declaration dec Nothing)   = T.pack (pprint dec)
renderDeclaration (Comment comment)           = T.unlines $ map (\line -> "-- " <> line) $ T.lines comment

-- | Haskell code (without module declaration and imports)
type Code = [Declaration]

type StringSet = HS.HashSet String

-- | Generates a fresh name
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

-- Code generation monad: Keeps a set of used names, writes out the code and
-- has a readonly map from schema identifiers to the names of the corresponding
-- types in the generated code.
newtype CodeGenM s a = CodeGenM
  { unCodeGenM :: RWST s Code StringSet Q a
  } deriving (Monad, Applicative, Functor, MonadReader s, MonadWriter Code, MonadState StringSet)

instance Quasi (CodeGenM s) where
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

instance MonadIO (CodeGenM s) where
  liftIO = qRunIO

-- ^ Generates a record data declaration where the fields may have descriptions for Haddock
genRecord :: Name -- ^ Type and constructor name
               -> [(Name, TypeQ, Maybe Text)] -- ^ Fields
               -> [Name] -- ^ Deriving typeclasses
               -> Q Declaration
genRecord name fields classes = Declaration <$> dataDec
                                            <*> (Just . recordBlock . map fieldLine <$> fields')
  where
    fields' :: Q [(Name, Type, Maybe Text)]
    fields' = mapM (\(fieldName, fieldType, fieldDesc) -> (fieldName,,fieldDesc) <$> fieldType) fields
    dataLine, derivingClause :: Text
    dataLine = "data " <> pack (nameBase name) <> " = " <> pack (nameBase name)
    derivingClause = "deriving (" <> T.intercalate ", " (map (\n -> maybe "" ((<> ".") . pack) (nameModule n) <> pack (nameBase n)) classes) <> ")"
    fieldLine :: (Name, Type, Maybe Text) -> Text
    fieldLine (fieldName, fieldType, fieldDesc) = mconcat
      [ pack (nameBase fieldName)
      , " :: "
      , pack (pprint fieldType)
      , maybe "" ((" " <>) . renderComment . ("^ " <>)) fieldDesc
      ]
    renderComment :: Text -> Text
    renderComment = T.unlines . map ("-- " <>) . T.lines
    recordBlock :: [Text] -> Text
    recordBlock [] = dataLine <> " " <> derivingClause
    recordBlock (l:ls) = T.unlines $ [dataLine] ++ map indent (["{ " <> l] ++ map (", " <>) ls ++ ["} " <> derivingClause])
    indent :: Text -> Text
    indent = ("  " <>)

    -- Template Haskell
    constructor = recC name $ map (\(fieldName, fieldType, _) -> (fieldName,NotStrict,) <$> fieldType) fields
    dataDec = dataD (cxt []) name [] [constructor] classes
