module Data.Aeson.Schema.Helpers
  ( vectorUnique
  , formatValidators
  , validateFormat
  , isDivisibleBy
  , replaceHiddenModules
  , cleanPatterns
  , getUsedModules
  ) where

import           Control.Monad          (join)
import           Data.Generics          (Data, everything, everywhere, mkQ, mkT)
import           Data.List              (nub)
import qualified Data.Map               as M
import           Data.Maybe             (maybeToList)
import           Data.Scientific        (Scientific, coefficient, base10Exponent)
import           Data.Text              (Text, unpack)
import qualified Data.Vector            as V
import           Language.Haskell.TH    (Name, Pat (..), mkName, nameBase,
                                         nameModule)
import           Text.Regex.PCRE        (makeRegexM)
import           Text.Regex.PCRE.String (Regex)

-- | Tests whether all items in a vector are different from each other.
vectorUnique :: (Eq a) => V.Vector a -> Bool
vectorUnique v = length (nub $ V.toList v) == V.length v

-- | List of format validators. Some validators haven't been implemented yet.
-- Those which are implemented take a Text value and return an error in case the
-- input is invalid.
formatValidators :: [(Text, Maybe (Text -> Maybe String))]
formatValidators =
  [ ("date-time", Nothing)
  , ("data", Nothing)
  , ("time", Nothing)
  , ("utc-millisec", Nothing)
  , ( "regex"
    , Just $ \str -> case makeRegexM (unpack str) :: Maybe Regex of
        Nothing -> Just $ "not a regex: " ++ show str
        Just _ -> Nothing
    )
  , ("color", Nothing) -- not going to implement this
  , ("style", Nothing) -- not going to implement this
  , ("phone", Nothing)
  , ("uri", Nothing)
  , ("email", Nothing)
  , ("ip-address", Nothing)
  , ("ipv6", Nothing)
  , ("host-name", Nothing)
  ]

-- | Validates a Text value against a format.
validateFormat :: Text -- ^ format
               -> Text -- ^ input
               -> Maybe String -- ^ message in case of an error
validateFormat format str = ($ str) =<< join (lookup format formatValidators)

-- | Tests whether the first number is divisible by the second with no remainder.
isDivisibleBy :: Scientific -> Scientific -> Bool
isDivisibleBy a b =
  let ca = coefficient a
      ea = base10Exponent a
      cb = coefficient b
      eb = base10Exponent b
  in if ea >= eb
     then (10 ^ (ea - eb)) * ca `mod` cb == 0
     else ca `mod` (10 ^ (eb - ea)) == 0

--isDivisibleBy (I i) (I j) = i `mod` j == 0
--isDivisibleBy a b = a == 0 || denominator (approxRational (a / b) epsilon) `elem` [-1,1]
  --where epsilon = D $ 10 ** (-10)

-- | Workaround for an issue in Template Haskell: when you quote a name in TH
-- like 'Text (Data.Text.Text) then TH searches for the module where Text is
-- defined, even if that module is not exported by its package (in this case
-- Text is defined in Data.Text.Internal). This works when we use TH to insert
-- some code in a module but not when we use the TH code for pretty-printing.
replaceHiddenModules :: Data a
                     => a -- ^ Dec or Exp
                     -> M.Map String String
                     -- ^ Extra replacements, supplied by 'CodeGenM'
                     -> a
replaceHiddenModules d replaceMap = everywhere (mkT replaceModule) d
  where
    replaceModule :: Name -> Name
    replaceModule n = case nameModule n of
      Just "Data.Aeson.Types.Internal" | nameBase n `elem` ["I", "D"] ->
        mkName $ "Data.Attoparsec.Number." ++ nameBase n
      Just "GHC.Tuple" -> mkName $ nameBase n
      Just m -> case M.lookup m replaceMap of
        Just r -> mkName $ r ++ ('.' : nameBase n)
        Nothing -> n
      _ -> n

-- | Workaround for a bug in Template Haskell: TH parses the empty list
-- constructor in patterns as @ConP (mkName \"Prelude.[]\") []@ instead of @ListP []@
cleanPatterns :: Data a => a -> a
cleanPatterns = everywhere $ mkT replacePattern
  where
    replacePattern (ConP n []) | nameBase n == "[]" = ListP []
    replacePattern p = p

-- | Extracts a list of used modules from a TH code tree.
getUsedModules :: Data a => a -> [String]
getUsedModules = nub . everything (++) ([] `mkQ` extractModule)
  where
    extractModule :: Name -> [String]
    extractModule = maybeToList . nameModule
