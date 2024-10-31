module Common 
  (
    module Relude
  -- , module Text.Show
  , nth
  ) where

import Relude hiding (Type)
-- import qualified Text.Show

nth :: [a] -> Int -> a -> a
nth [] _ def = def
nth (x:_) 0 _ = x
nth (_:xs) i def = nth xs (i-1) def