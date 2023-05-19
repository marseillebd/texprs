module Data.Tree.Template
  ( Template(..)
  , rewrite
  ) where

import Prelude hiding (lookup)

import Control.Applicative (Alternative(..))
import Data.Tree.Match (Var,Match,MatchValue(..))

import qualified Data.Map as Map

data Template a
  = Var Var
  -- -| Const a
  -- -| Replace Var (a -> [a] -> Maybe a) [Template a] -- ^ use the function to replace the children of the first arg, if possible
  | Combo ([a] -> a) [Template a]
  | Repeat ([a] -> a) [Template a]

rewrite :: [Template a] -> Match a -> Maybe [a]
rewrite ts0 m0 = (unRW (rewriteMany ts0)) m0 []

rewriteOne :: Template a -> Rewriter a a
rewriteOne (Var x) = lookup x
rewriteOne (Combo f ts) = f <$> rewriteMany ts
rewriteOne (Repeat f ts) =
  let loop = ((<>) <$> rewriteMany ts <*> loop) <|> pure []
   in f <$> loop

rewriteMany :: [Template a] -> Rewriter a [a]
rewriteMany [] = pure []
rewriteMany (t:ts) = (:) <$> rewriteOne t <*> rewriteMany ts

------ Monad ------

newtype Rewriter v a = RW { unRW :: Match v -> [Int] -> Maybe a }

instance Functor (Rewriter v) where
  fmap f x = RW $ \m ixs -> f <$> unRW x m ixs

instance Applicative (Rewriter v) where
  pure x = RW $ \_ _ -> Just x
  a <*> b = RW $ \m ixs ->
    let f = unRW a m ixs
        x = unRW b m ixs
     in f <*> x

instance Alternative (Rewriter v) where
  empty = RW $ \_ _ -> Nothing
  a <|> b = RW $ \m ixs -> unRW a m ixs <|> unRW b m ixs

lookup :: Var -> Rewriter v v
lookup x = RW $ \m0 ixs0 -> Map.lookup x m0 >>= loop ixs0
  where
  loop :: [Int] -> MatchValue v -> Maybe v
  loop [] (One v) = Just v
  loop (ix:ixs) (Many vs) = case drop ix vs of
    [] -> Nothing
    (m:_) -> loop ixs m
  loop [] (Many _) = Nothing
  loop (_:_) (One _) = Nothing
