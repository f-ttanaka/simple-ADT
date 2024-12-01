{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Core.Typing.Infer where

import Common
import Control.Exception.Safe (MonadThrow, throwString)
import Core.Env
import           Core.Expr
import           Core.Type
import           Core.Typing.Subst
import qualified Data.Map          as M
import qualified Data.Set          as S

type TIEnv = (TyEnv, ConstructorEnv)
type InferState = Int
newtype Infer m a = Infer (ReaderT TIEnv (StateT InferState m) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader TIEnv
    , MonadState InferState
    , MonadThrow)

-- operations on env

initialInferState :: InferState
initialInferState = 0

askTyEnv :: Monad m => Infer m TyEnv
askTyEnv = fst <$> ask

askCEnv :: MonadThrow m => Infer m ConstructorEnv
askCEnv = snd <$> ask

localTyEnv :: Monad m => (TyEnv -> TyEnv) -> Infer m a -> Infer m a
localTyEnv f = local (first f)

--- inference processes

freshVar :: MonadThrow m => Infer m Type
freshVar = do
  i <- get
  put (succ i)
  return $ TyVar i

instanciate :: MonadThrow m => Scheme -> Infer m Type
instanciate (Forall xs t) = do
  let tvs = S.toList xs
  tvs' <- mapM (const freshVar) tvs
  let sub = Subst $ M.fromList (zip tvs tvs')
  return $ apply sub t

generalize :: MonadThrow m => Type -> Infer m Scheme
generalize ty = do
  tEnv <- askTyEnv
  let xs = ftv ty `S.difference` ftv tEnv
  return $ Forall xs ty

inferType :: MonadThrow m => Expr -> Infer m (Type, Subst)
inferType (EVar x) = do
  sc <- lookupTyEnv x =<< askTyEnv
  ty <- instanciate sc
  return (ty,mempty)
inferType (ETag tag) = do
  info <- lookupCInfo tag =<< askCEnv
  ty <- instanciate (constructorType info)
  return (ty,mempty)
inferType (EAbs x e) = do
  tv <- freshVar
  (tBody,sub) <- localTyEnv (insertTyEnv x (Forall mempty tv)) (inferType e)
  return (apply sub tv `tyFunc` tBody, sub)
inferType (EApp e1 e2) = do
  tv <- freshVar
  (tf,sub1) <- inferType e1
  (ta,sub2) <- local (first $ apply sub1) (inferType e2)
  sub3 <- unify (apply sub2 tf) (ta `tyFunc` tv)
  return (apply sub3 tv, sub3 <> sub2 <> sub1)
inferType (ECase e ms) = do
  (te, subE) <- inferType e
  branchResults <- local (first $ apply subE) $ mapM (inferCase te) ms
  ret <- freshVar
  subs <- mapM (\(branchRet, branchSub) -> local (first $ apply branchSub) $ unify branchRet ret) branchResults
  return (ret, fold subs)

inferCase :: MonadThrow m => Type -> (Pat,Expr) -> Infer m (Type, Subst)
inferCase scrutinee (p,e) = do
  tv <- freshVar
  sub1 <- inspectPattern tv p
  (tyE, sub2) <- local (first $ apply sub1) (inferType e)
  sub3 <- local (first $ apply (sub2 <> sub1)) (unify scrutinee tv)
  return (apply sub3 tyE, sub3 <> sub2 <> sub1)
  where
    inspectPattern :: MonadThrow m => Type -> Pat -> Infer m Subst
    inspectPattern _ PWildcard = return mempty
    inspectPattern scr (PCons c ps) = do
      pVars <- mapM (const freshVar) ps
      cInfo <- lookupCInfo c =<< askCEnv
      let pType = foldr tyFunc scr pVars
      cType <- instanciate (constructorType cInfo)
      unify pType cType

-- start to execute inference monad

inferScheme :: MonadThrow m => Expr -> Infer m Scheme
inferScheme e = do
  (t,_) <- inferType e
  generalize t

runInfer :: MonadThrow m => Expr -> TyEnv -> ConstructorEnv -> m Scheme
runInfer e tEnv cEnv = evalStateT (runReaderT m (tEnv,cEnv)) initialInferState
  where
    Infer m = inferScheme e
