module Data.Type where

import qualified Data.Set  as S (Set)

type TyVar = String

data Type = TVar TyVar
  | TBase String
  | TArrow Type Type
  deriving Eq

tInt, tBool :: Type
tInt = TBase "int"
tBool = TBase "bool"

data Scheme = Forall (S.Set TyVar) Type

instance Show Type where
  show (TVar x) = x
  show (TBase bt) = bt
  show (TArrow t1 t2)
    | TArrow _ _ <- t1 = "(" ++ show t1 ++ ")" ++ " -> " ++ show t2
    | otherwise = show t1 ++ " -> " ++ show t2

instance Show Scheme where
  show (Forall _ t) = show t
