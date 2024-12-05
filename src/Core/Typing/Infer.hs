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

inferType :: MonadThrow m => Expr -> Infer m Type
inferType e = do
  tyExp <- freshVar
  sub <- checkType tyExp e
  return $ apply sub tyExp

checkType :: MonadThrow m => Type -> Expr -> Infer m Subst
checkType tyExp (EVar x) = do
  sc <- lookupTyEnv x =<< askTyEnv
  ty <- instanciate sc
  unify tyExp ty
checkType tyExp (ETag tag) = do
  info <- lookupCInfo tag =<< askCEnv
  ty <- instanciate (constructorType info)
  unify tyExp ty
checkType tyExp (EAbs x e) = do
  tVar <- freshVar
  tBody <- freshVar
  sub1 <- unify tyExp (tVar `tyFunc` tBody)
  scVar <- generalize (apply sub1 tVar)
  sub2 <- localTyEnv (apply sub1 . insertTyEnv x scVar) (checkType (apply sub1 tBody) e)  
  return $ sub2 <> sub1
checkType tyExp (EApp e1 e2) = do
  tv <- freshVar
  sub1 <- checkType (tv `tyFunc` tyExp) e1
  sub2 <- localTyEnv (apply sub1) (checkType (apply sub1 tv) e2)
  return $ sub2 <> sub1
checkType tyExp (ECase e ms) = do
  tyPat <- freshVar
  subE <- checkType tyPat e
  subs <- local (first $ apply subE) $ mapM (checkCase (apply subE tyPat) tyExp) ms
  return $ fold (reverse subs)

-- receives expected types of pattern and return expr
checkCase :: MonadThrow m => Type -> Type -> (Pat,Expr) -> Infer m Subst
checkCase tyPat tyRet (p,e) = do
  (sub1,binds) <- checkPattern tyPat p
  sub2 <- localTyEnv (insertsTyEnv binds . apply sub1) (checkType (apply sub1 tyRet) e)
  return $ sub2 <> sub1
  where
    checkPattern :: MonadThrow m => Type -> Pat -> Infer m (Subst, [(Var,Scheme)])
    checkPattern _ PWildcard = return mempty
    checkPattern scr (PCons c ps) = do
      pVars <- mapM (const freshVar) ps
      cInfo <- lookupCInfo c =<< askCEnv
      let pType = foldr tyFunc scr pVars
      cType <- instanciate (constructorType cInfo)
      pScs <- mapM generalize pVars
      (,zip ps pScs) <$> unify pType cType

-- start to execute inference monad

inferScheme :: MonadThrow m => Expr -> Infer m Scheme
inferScheme e = generalize =<< inferType e

runInfer :: MonadCatch m => Expr -> TyEnv -> ConstructorEnv -> [Var] -> m Scheme
runInfer e tEnv cEnv xs = evalStateT (runReaderT m (tEnv,cEnv)) initialInferState
  where
    Infer m = do
      tVars <- mapM (const $ generalize =<< freshVar) xs
      localTyEnv (insertsTyEnv (zip xs tVars)) (inferScheme e)
