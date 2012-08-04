{-# LANGUAGE TemplateHaskell #-}

module Data.Aeson.Schema.Choice
  ( generateChoice
  ) where

import Control.Monad (mapM)
import Language.Haskell.TH
import Data.Aeson (ToJSON (..), FromJSON (..))
import Control.Applicative (Alternative (..))

generateChoice :: Int -> Q [Dec]
generateChoice n | n < 2 = return []
generateChoice n = do
  tyName <- newName $ "Choice" ++ show n
  let tyParamNames = map (mkName . singleton) $ take n ['a'..]
  let tyParams = map varT tyParamNames
  conNames <- mapM newName $ map (\i -> "Choice" ++ show i ++ "of" ++ show n) [1..n]
  let cons = zipWith normalC conNames $ map ((:[]) . strictType notStrict) tyParams
  dataDec <- dataD (cxt []) tyName (map PlainTV tyParamNames) cons [''Eq, ''Ord, ''Show, ''Read]
  let tyCon = foldl appT (conT tyName) tyParams
  let genClassConstraints c = cxt $ map (classP c . singleton) tyParams
  instToJSON <- instanceD (genClassConstraints ''ToJSON)
                          (appT (conT ''ToJSON) tyCon)
                          [ funD 'toJSON $ zipWith genToJSONClause conNames tyParamNames ]
  instFromJSON <- instanceD (genClassConstraints ''FromJSON)
                            (appT (conT ''FromJSON) tyCon)
                            [ let v = mkName "v" in
                              funD 'parseJSON [clause [varP v]
                                                      (normalB $ foldl (\a b -> [e|(<|>)|] `appE` a `appE` b) (varE 'empty)
                                                               $ map (\con -> varE 'fmap `appE` conE con `appE` (varE 'parseJSON `appE` (varE v))) conNames)
                                                      []
                                              ]
                            ]
  choiceFun <- funD (mkName $ "choice" ++ show n)
                  $ let f = mkName "f"
                        v = mkName "v"
                    in zipWith (\i con -> clause (replicate i wildP ++ [varP f] ++ replicate (n-i-1) wildP ++ [conP con [varP v]])
                                                 (normalB $ conE con `appE` (varE f `appE` varE v))
                                                 []) [0..] conNames
  return [dataDec, instToJSON, instFromJSON, choiceFun]
  where
    singleton :: a -> [a]
    singleton = (:[])
    genToJSONClause :: Name -> Name -> ClauseQ
    genToJSONClause con param = clause [conP con [varP param]] (normalB . appE (varE 'toJSON) . varE $ param) []
