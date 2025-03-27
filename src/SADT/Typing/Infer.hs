{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module SADT.Typing.Infer where

import qualified Data.Map as M
import qualified Data.Set as S
import SADT.Common
import SADT.Data.Env
import SADT.Data.Expr
import SADT.Data.Type
import SADT.Typing.Subst

type TIEnv = (TyEnv, ConstructorEnv)

type InferState = Int

newtype Infer m a = Infer (ReaderT TIEnv (StateT InferState m) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader TIEnv,
      MonadState InferState,
      MonadThrow
    )

-- operations on env

initialInferState :: InferState
initialInferState = 0

askTyEnv :: (Monad m) => Infer m TyEnv
askTyEnv = fst <$> ask

askCEnv :: (MonadThrow m) => Infer m ConstructorEnv
askCEnv = snd <$> ask

localTyEnv :: (Monad m) => (TyEnv -> TyEnv) -> Infer m a -> Infer m a
localTyEnv f = local (first f)

findTy :: (MonadThrow m) => Var -> Infer m Type
findTy x = do
  sc <- lookupTyEnv x =<< askTyEnv
  instanciate sc

findConsTy :: (MonadThrow m) => Tag -> Infer m Type
findConsTy t = do
  cInfo <- lookupCInfo t =<< askCEnv
  instanciate (constructorType cInfo)

--- inference processes

freshVar :: (MonadThrow m) => Infer m Type
freshVar = do
  i <- get
  put (succ i)
  return $ TyVar i

freshScheme :: (MonadThrow m) => Infer m Scheme
freshScheme = Forall mempty <$> freshVar

instanciate :: (MonadThrow m) => Scheme -> Infer m Type
instanciate (Forall xs t) = do
  let tvs = S.toList xs
  tvs' <- mapM (const freshVar) tvs
  let sub = Subst $ M.fromList (zip tvs tvs')
  return $ apply sub t

generalize :: (MonadThrow m) => Type -> Infer m Scheme
generalize ty = do
  tEnv <- askTyEnv
  let xs = ftv ty `S.difference` ftv tEnv
  return $ Forall xs ty

inferType :: (MonadThrow m) => Expr -> Infer m (Type, [Constraint])
inferType (EVar x) = (,[]) <$> findTy x
inferType (ETag tag) = (,[]) <$> findConsTy tag
inferType (EAbs var body) = do
  tVar <- freshVar
  (tBody, c) <- localTyEnv (insertTyEnv var (Forall mempty tVar)) (inferType body)
  return (tVar `tyFunc` tBody, c)
inferType (EApp eFun eArg) = do
  tyRet <- freshVar
  (tyFun, c1) <- inferType eFun
  (tyArg, c2) <- inferType eArg
  return (tyRet, c2 ++ c1 ++ [(tyFun, tyArg `tyFunc` tyRet)])
inferType (ECase e cases) = do
  tyRes <- freshVar
  (tyE, c1) <- inferType e
  inferred <- mapM (checkCase tyE) cases
  let c2 = join [cCase | (_, cCase) <- inferred]
      c3 = [(tyRes, tyCase) | (tyCase, _) <- inferred]
  return (tyRes, c3 <> c2 <> c1)

-- receives expected types of pattern and return expr
checkCase :: (MonadThrow m) => Type -> (Pat, Expr) -> Infer m (Type, [Constraint])
checkCase ty (pat, ex) = do
  tyVar <- freshVar
  (cs1, binds) <- checkPattern tyVar pat
  (tyEx, cs2) <- localTyEnv (insertsTyEnv binds) (inferType ex)
  return (tyEx, ((tyVar, ty) : cs1) ++ cs2)
  where
    checkPattern :: (MonadThrow m) => Type -> Pat -> Infer m ([Constraint], [(Var, Scheme)])
    checkPattern _ PWildcard = return mempty
    checkPattern scr (PVar x) = do
      sc <- generalize scr
      return (mempty, [(x, sc)])
    checkPattern scr (PCons c ps) = do
      pVars <- mapM (const freshVar) ps
      cInfo <- lookupCInfo c =<< askCEnv
      (subCs, subBinds) <- fold <$> zipWithM checkPattern pVars ps
      let pType = foldr tyFunc scr pVars
      cType <- instanciate (constructorType cInfo)
      return ((pType, cType) : subCs, subBinds)

-- start to execute inference monad

inferScheme :: (MonadThrow m) => Expr -> Infer m Scheme
inferScheme e = do
  (ty, cs) <- inferType e
  sub <- solve cs
  generalize (apply sub ty)

inferWithInitialBinds :: (MonadCatch m) => [Var] -> Expr -> Infer m Scheme
inferWithInitialBinds xs e = do
  tVars <- mapM (const $ generalize =<< freshVar) xs
  localTyEnv (insertsTyEnv (zip xs tVars)) (inferScheme e)

runInfer :: (MonadCatch m) => Expr -> TyEnv -> ConstructorEnv -> [Var] -> m Scheme
runInfer e tEnv cEnv xs = evalStateT (runReaderT m (tEnv, cEnv)) initialInferState
  where
    Infer m = inferWithInitialBinds xs e
