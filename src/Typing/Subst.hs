module Typing.Subst where

import Data.Expr
import Data.Type
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Subst = M.Map Name Type
type Equation = (Type, Type)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> S.Set Name

instance (Substitutable a, Substitutable b) => Substitutable (a,b) where
  apply sub (x,y) = (apply sub x, apply sub y)
  ftv (x,y) = S.union (ftv x) (ftv y)

instance Substitutable a => Substitutable [a] where
  apply sub xs = [apply sub x | x <- xs]
  ftv xs = S.unions [ftv x | x <- xs]

instance Substitutable Type where
  apply sub t@(TVar x)     = M.findWithDefault t x sub
  apply sub (TArrow t1 t2) = TArrow (apply sub t1) (apply sub t2)
  apply _ ty               = ty
  ftv (TVar x)       = S.singleton x
  ftv (TArrow t1 t2) = S.union (ftv t1) (ftv t2)
  ftv _              = mempty

instance Substitutable Scheme where
  apply sub (Forall xs t) = Forall xs (apply sub' t)
    where
      sub' = foldr M.delete sub xs
  ftv (Forall xs t) = S.difference (ftv t) xs

instance Substitutable a => Substitutable (M.Map v a) where
  apply sub = M.map (apply sub)
  ftv env = S.unions [ftv x | x <- M.elems env]

emptySubst :: Subst
emptySubst = mempty

compose :: Subst -> Subst -> Subst
compose sub1 sub2 = M.union (M.map (apply sub1) sub2) sub1

occurs :: Name -> Type -> Bool
occurs x (TVar y)       = x == y
occurs x (TArrow t1 t2) = occurs x t1 || occurs x t2
occurs _ _              = False

solve :: [Equation] -> Subst -> Maybe Subst
solve [] sub = Just sub
solve ((t1,t2):eqs) sub | t1 == t2 = solve eqs sub
solve ((TArrow t1 t2, TArrow t1' t2'):eqs) sub = solve ((t1,t1'):(t2,t2'):eqs) sub
solve ((TVar x, t2):eqs) sub
  | occurs x t2 = Nothing
  | otherwise = let sub' = M.fromList [(x,t2)] in
      solve (apply sub' eqs) (compose sub' sub)
solve ((t1, TVar y):eqs) sub = solve ((TVar y, t1):eqs) sub
solve _ _ = Nothing
