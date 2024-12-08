module Common 
  (
    module Relude
  , module Control.Exception.Safe
  -- , module Text.Show
  , nth
  , hasDuplicates
  , (<||>)
  ) where

import Control.Exception.Safe
import Relude hiding (Type, Constraint)
import qualified Data.Set as S

nth :: [a] -> Int -> a -> a
nth [] _ def = def
nth (x:_) 0 _ = x
nth (_:xs) i def = nth xs (i-1) def

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates = go mempty
  where
    go _ []     = False
    go seen (x:xs)
      | x `S.member` seen = True
      | otherwise         = go (S.insert x seen) xs

(<||>) :: MonadCatch m => m a -> m a -> m a
m1 <||> m2 = m1 `catch` \(SomeException _) -> m2