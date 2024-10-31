module Typing.Subst where

import Common
import Data.Expr
import Data.Type
import qualified Data.Map as M
import qualified Data.Set as S

newtype Subst = Subst {subst :: M.Map Var Type}
type Equation = (Type, Type)

instance Semigroup Subst where
  s1@(Subst sub1) <> (Subst sub2) = Subst $ M.map (apply s1) sub2 <> sub1

instance Monoid Subst where
  mempty = Subst mempty

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> S.Set Var

instance (Substitutable a, Substitutable b) => Substitutable (a,b) where
  apply sub (x,y) = (apply sub x, apply sub y)
  ftv (x,y) = S.union (ftv x) (ftv y)

instance Substitutable a => Substitutable [a] where
  apply sub xs = [apply sub x | x <- xs]
  ftv xs = S.unions [ftv x | x <- xs]

instance Substitutable Type where
  apply (Subst sub) t@(TVar x)     = M.findWithDefault t x sub
  apply sub (TArrow t1 t2) = TArrow (apply sub t1) (apply sub t2)
  apply _ ty               = ty
  ftv (TVar x)       = S.singleton x
  ftv (TArrow t1 t2) = S.union (ftv t1) (ftv t2)
  ftv _              = mempty

instance Substitutable Scheme where
  apply (Subst sub) (Forall xs t) = Forall xs (apply s' t)
    where
      s' = Subst $ foldr M.delete sub xs
  ftv (Forall xs t) = S.difference (ftv t) xs

instance Substitutable a => Substitutable (M.Map v a) where
  apply sub = M.map (apply sub)
  ftv env = S.unions [ftv x | x <- M.elems env]

occursIn :: Var -> Type -> Bool
occursIn x (TVar y)       = x == y
occursIn x (TArrow t1 t2) = occursIn x t1 || occursIn x t2
occursIn _ _              = False

solve :: [Equation] -> Subst -> Maybe Subst
solve [] sub = Just sub
solve ((t1,t2):eqs) sub | t1 == t2 = solve eqs sub
solve ((TArrow t1 t2, TArrow t1' t2'):eqs) sub = solve ((t1,t1'):(t2,t2'):eqs) sub
solve ((TVar x, t2):eqs) sub
  | x `occursIn` t2 = Nothing
  | otherwise = let sub' = Subst $ M.fromList [(x,t2)] in
      solve (apply sub' eqs) (sub' <> sub)
solve ((t1, TVar y):eqs) sub = solve ((TVar y, t1):eqs) sub
solve _ _ = Nothing
