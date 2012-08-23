module Data.Aeson.Schema.Helpers
  ( vectorUnique
  , formatValidators
  , validateFormat
  , isDivisibleBy
  ) where

import qualified Data.Vector as V
import Data.List (nub)
import Control.Monad (join)
import Data.Text (Text, unpack)
import Text.Regex.PCRE (makeRegexM)
import Text.Regex.PCRE.String (Regex)
import Data.Attoparsec.Number (Number (..))
import Data.Ratio (denominator, approxRational)

vectorUnique :: (Eq a) => V.Vector a -> Bool
vectorUnique v = length (nub $ V.toList v) == V.length v

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

validateFormat :: Text -> Text -> Maybe String
validateFormat format str = ($ str) =<< join (lookup format formatValidators)

isDivisibleBy :: Number -> Number -> Bool
isDivisibleBy (I i) (I j) = i `mod` j == 0
isDivisibleBy a b = a == fromInteger 0 || denominator (approxRational (a / b) epsilon) `elem` [-1,1]
  where epsilon = D $ 10 ** (-10)
