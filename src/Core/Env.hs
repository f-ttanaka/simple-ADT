module Core.Env where

import Common
import Control.Exception.Safe
import Core.Expr
import Core.Type
import qualified Data.Map as M

newtype TyEnv = TyEnv (Map Var Scheme)
  deriving (Semigroup, Monoid, Show)

lookupTyEnv :: MonadThrow m => Var -> TyEnv -> m Scheme
lookupTyEnv x (TyEnv env) = case M.lookup x env of
  Just sc -> return sc
  _ -> throwString $ show x ++ " is not in type env."

insertTyEnv :: Var -> Scheme -> TyEnv -> TyEnv
insertTyEnv x sc (TyEnv env) = TyEnv $ M.insert x sc env

data ConstructorInfo = CInfo
  { constructorType :: Scheme
  , patternType :: Tag }
  deriving (Show)
newtype ConstructorEnv = ConstructorEnv (Map Tag ConstructorInfo)
  deriving (Semigroup, Monoid, Show)

lookupCInfo :: MonadThrow m => Tag -> ConstructorEnv -> m ConstructorInfo
lookupCInfo tag (ConstructorEnv env) = case M.lookup tag env of
  Just info -> return info
  _ -> throwString $ show tag ++ " is not in constructor env."

insertCEnv :: Tag -> Scheme -> Tag -> ConstructorEnv -> ConstructorEnv
insertCEnv t sc pt (ConstructorEnv env) = 
  ConstructorEnv $ M.insert t info env
  where
    info = CInfo 
      {
        constructorType = sc
      , patternType = pt
      }