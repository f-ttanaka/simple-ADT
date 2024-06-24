module Typing.Prim where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Type
import Typing.Infer

arithBinOpType :: Scheme
arithBinOpType = Forall mempty $ tInt `TArrow` (tInt `TArrow` tInt)

compBinOpType :: Scheme
compBinOpType = Forall mempty $ tInt `TArrow` (tInt `TArrow` tBool)

initialTEnv :: TEnv
initialTEnv = M.fromList [
    ("+", arithBinOpType)
  , ("-", arithBinOpType)
  , ("*", arithBinOpType)
  , ("/", arithBinOpType)
  , ("=", compBinOpType)
  , (">", compBinOpType)
  , ("<", compBinOpType)
  , ("if", Forall (S.singleton "a") $ tBool `TArrow` (TVar "a" `TArrow` (TVar "a" `TArrow` TVar "a")))
  ]