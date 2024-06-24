module Typing.Infer where

import Control.Monad.Check (Check)
import Control.Monad (replicateM, foldM)
import Control.Monad.Reader ( MonadReader(local, ask) )
import           Control.Monad.RS
import Control.Monad.State ( modify, MonadState(get) )
import           Data.Expr
import qualified Data.Map          as M
import qualified Data.Set          as S
import           Data.Type
import           Typing.Subst

type TEnv = M.Map Name Scheme
type InferState = Int
type Infer a = RST TEnv InferState Check a

initialInferState :: InferState
initialInferState = 0

unify :: Type -> Type -> Infer Subst
unify t1 t2 = case solve [(t1,t2)] mempty of
  Just sub -> return sub
  _        -> fail $ "unification fail: " ++ show t1 ++ " " ++ show t2

typeVars :: [String]
typeVars = [x | n <- [1..], x <- replicateM n ['a'..'z']]

freshVar :: Infer Type
freshVar = do
  i <- get
  modify succ
  return $ TVar (typeVars !! i)

instanciate :: Scheme -> Infer Type
instanciate (Forall xs t) = do
  let tvs = S.toList xs
  tvs' <- mapM (const freshVar) tvs
  let sub = M.fromList (zip tvs tvs')
  return (apply sub t)

generalize :: Type -> Infer Scheme
generalize t = do
  env <- ask
  let xs = S.difference (ftv t) (ftv env)
  return (Forall xs t)

inEnv :: [(Name,Scheme)] -> Infer a -> Infer a
inEnv bs = local scope
  where
    scope :: TEnv -> TEnv
    scope env = foldr (\(x,sc) e -> M.insert x sc e) env bs

lookupTEnv :: Name -> Infer Type
lookupTEnv x = do
  env <- ask
  case M.lookup x env of
    Nothing -> fail $ "variable not found: " ++ x
    Just sc -> instanciate sc

--- inference processes

inferLit :: Lit -> Infer Type
inferLit LInt{} = return tInt

inferBind :: ([Scheme], Subst) -> Expr -> Infer ([Scheme], Subst)
inferBind (scs,sub) e = do
  (t,sub') <- local (apply sub) (infer e)
  sc <- generalize t
  return (scs ++ [sc], sub' `compose` sub)

inferBinds :: [(Name,Expr)] -> Infer ([(Name,Scheme)], Subst)
inferBinds binds = do
  let (xs,es) = unzip binds
  tvs <- mapM (const freshVar) binds
  let xBinds = [(x, Forall mempty tv) | (x,tv) <- zip xs tvs]
  (_,sub) <- inEnv xBinds (foldM inferBind mempty es)
  return ([(x, apply sub t) | (x,t) <- xBinds], sub)

infer :: Expr -> Infer (Type, Subst)
infer (ELit l) = (,emptySubst) <$> inferLit l
infer (EVar x) = (,emptySubst) <$> lookupTEnv x
infer (EAbs x e) = do
  tv <- freshVar
  (tBody,sub) <- local (M.insert x (Forall mempty tv)) (infer e)
  return (apply sub tv `TArrow` tBody, sub)
infer (EApp e1 e2) = do
  tv <- freshVar
  (t1,sub1) <- infer e1
  (t2,sub2) <- local (apply sub1) (infer e2)
  sub3 <- unify (apply sub2 t1) (t2 `TArrow` tv)
  return (apply sub3 tv, sub3 `compose` sub2 `compose` sub1)
infer (ELet binds body) = do
  (bs,sub) <- inferBinds binds
  let (xs,scs) = unzip bs
  inEnv (zip xs (apply sub scs)) (infer body)

inferScheme :: Expr -> Infer Scheme
inferScheme e = do
  (t,_) <- infer e
  generalize t

-- runInferLetBinds :: IsRec -> TEnv -> [(Name,Expr)] -> Either String [(Name,Scheme)]
-- runInferLetBinds isRec env binds = do
--   let inferM = if isRec then inferRecBinds binds else inferBinds binds
--   ((sBinds,_),_, _) <- runRWST inferM env 0
--   return sBinds

runInfer :: Infer Scheme -> TEnv -> Check Scheme
runInfer m env = evalRST m env initialInferState
