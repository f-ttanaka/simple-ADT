module Core.Type where

import Common

import Core.Expr (Tag)
import qualified Text.Show
import qualified Data.String as String (unwords)

type TyName = String
type Uniq = Int

data Type = 
    TyVar Uniq
  | TyCon Tag [Type]
  deriving Eq
data Scheme = Forall (Set Uniq) Type

instance Show Type where
  show (TyVar x) = show x
  show (TyCon tag ts) = String.unwords $ tag : [show t | t <- ts]

instance Show Scheme where
  show (Forall xs t) = 
    if xs == mempty
      then show t
      else "∀. " ++ intercalate "," [showTyVar x | x <- toList xs] ++ ", " ++ show t

typeVars :: [String]
typeVars = [x | n <- [1..], x <- replicateM n ['a'..'z']]

showTyVar :: Uniq -> String
showTyVar i = nth typeVars i "!?"

tyFunc :: Type -> Type -> Type
tyFunc t1 t2 = TyCon "Func" [t1, t2]