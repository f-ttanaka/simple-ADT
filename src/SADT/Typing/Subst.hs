module SADT.Typing.Subst where

import qualified Data.Map as M
import qualified Data.Set as S
import SADT.Common
import SADT.Data.Env
import SADT.Data.Type

newtype Subst = Subst {subst :: M.Map Uniq Type}

type Constraint = (Type, Type)

instance Semigroup Subst where
  s1@(Subst sub1) <> (Subst sub2) = Subst $ M.map (apply s1) sub2 <> sub1

instance Monoid Subst where
  mempty = Subst mempty

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> S.Set Uniq

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  apply sub (x, y) = (apply sub x, apply sub y)
  ftv (x, y) = S.union (ftv x) (ftv y)

instance (Substitutable a) => Substitutable [a] where
  apply sub xs = [apply sub x | x <- xs]
  ftv xs = S.unions [ftv x | x <- xs]

instance Substitutable Type where
  apply (Subst s) t@(TyVar x) = M.findWithDefault t x s
  apply sub (TyCon tag ts) = TyCon tag [apply sub t | t <- ts]
  ftv (TyVar x) = S.singleton x
  ftv (TyCon _ ts) = S.unions [ftv t | t <- ts]

instance Substitutable Scheme where
  apply (Subst s) (Forall xs t) = Forall xs (apply s' t)
    where
      s' = Subst $ foldr M.delete s xs
  ftv (Forall xs t) = S.difference (ftv t) xs

instance Substitutable TyEnv where
  apply sub (TyEnv env) = TyEnv $ M.map (apply sub) env
  ftv (TyEnv env) = S.unions [ftv x | x <- M.elems env]

-- unification

occursIn :: Uniq -> Type -> Bool
occursIn x ty = x `S.member` ftv ty

unify :: (MonadThrow m) => Type -> Type -> m Subst
unify (TyVar x) ty = unifyVar x ty
unify ty (TyVar x) = unifyVar x ty
unify (TyCon t1 ts1) (TyCon t2 ts2)
  | t1 == t2 = unifyMany ts1 ts2
  | otherwise = throwString "unify failed."

unifyVar :: (MonadThrow m) => Uniq -> Type -> m Subst
unifyVar x ty
  | TyVar x == ty = return mempty
  | x `occursIn` ty = throwString $ "Infinite type: '" ++ show ty ++ "'"
  | otherwise = return $ Subst (M.singleton x ty)

unifyMany :: (MonadThrow m) => [Type] -> [Type] -> m Subst
unifyMany [] [] = return mempty
unifyMany (t1 : ts1) (t2 : ts2) = do
  sub <- unify t1 t2
  sub' <- unifyMany (apply sub ts1) (apply sub ts2)
  return $ sub' <> sub
unifyMany ts1 ts2 = throwString $ "unification mismatch: " ++ show ts1 ++ show ts2

solve :: (MonadThrow m) => [Constraint] -> m Subst
solve = solveIter mempty
  where
    solveIter :: (MonadThrow m) => Subst -> [Constraint] -> m Subst
    solveIter sub [] = return sub
    solveIter sub ((t1, t2) : cs) = do
      sub' <- unify t1 t2
      solveIter (sub' <> sub) (apply sub' cs)
