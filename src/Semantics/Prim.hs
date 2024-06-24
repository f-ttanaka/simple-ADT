module Semantics.Prim where

import Data.Expr
import qualified Data.Map as M
import Semantics.Eval

numericBinOpPrim :: (Int -> Int -> Int) -> Val
numericBinOpPrim op = VPrim $ \e1 -> return $ VPrim $ \e2 -> do
  n <- unpackInt e1
  m <- unpackInt e2
  return $ VInt (op n m)

numericBinCompPrim :: (Int -> Int -> Bool) -> Val
numericBinCompPrim op = VPrim $ \e1 -> return $ VPrim $ \e2 -> do
  n <- unpackInt e1
  m <- unpackInt e2
  return $ VBool (op n m)

ifPrim :: Val
ifPrim = VPrim $ \e1 -> return $ VPrim $ \e2 -> return $ VPrim $ \e3 -> do
  b <- unpackBool e1
  if b then eval e2 else eval e3

primitiveOps :: [(Name, Val)]
primitiveOps =
  [
    ("+", numericBinOpPrim (+))
  , ("-", numericBinOpPrim (-))
  , ("*", numericBinOpPrim (*))
  , ("/", numericBinOpPrim div)
  , ("<", numericBinCompPrim (<))
  , (">", numericBinCompPrim (>))
  , ("=", numericBinCompPrim (==))
  , ("if", ifPrim)
  ]

primitives :: [(Name, Val)]
primitives = primitiveOps

initialVEnv :: VEnv
initialVEnv = M.fromList primitives