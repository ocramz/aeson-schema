{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aeson.Schema.CodeGenM
  ( Declaration (..)
  , Code
  , CodeGenM (..)
  , codeGenNewName
  ) where

import           Control.Applicative        (Applicative (..))
import           Control.Monad.RWS.Lazy     (MonadReader (..), MonadState (..),
                                             MonadWriter (..), RWST (..))
import qualified Control.Monad.Trans.Class  as MT
import           Data.Data                  (Data, Typeable)
import           Data.Function              (on)
import qualified Data.HashSet               as HS
import           Data.Text                  (Text)
import           Language.Haskell.TH.Syntax

-- | A top-level declaration.
data Declaration = Declaration Dec (Maybe Text) -- ^ Optional textual declaration. This can be used for information (e.g. inline comments) that are not representable in TH.
                 | Comment Text -- ^ Comment text
                 deriving (Show, Eq, Typeable, Data)

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
