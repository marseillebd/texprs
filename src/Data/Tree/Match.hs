{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tree.Match
  ( Var
  , Pattern(..)
  , Match
  , MatchValue(..)
  , match
  , search
  ) where

import Data.Bifunctor (first)
import Data.Map (Map)

import qualified Data.Map as Map

type Var = String

data Pattern a
  = Var Var -- ^ technically redundant 'Var x === Sat (Just x) (const True)', but may be useful declaratively
  | Sat (Maybe Var) (a -> Bool)
  -- ^ match a single input that satisfies the predicate,
  -- possibly binding the input to a name
  | Rec (Maybe Var) (a -> Maybe [a]) [Pattern a]
  -- ^ unpack the children of an input (if possible) and match them against the patterns,
  -- possibly binding the parent node to a name
  | ManyLeft (Pattern a) -- ^ match the first pattern up to the first occurrence of the second
  | ManyRight (Pattern a) -- ^ match the first pattern up to the last occurrence of the second

type Match a = Map Var (MatchValue a)
data MatchValue a
  = One a
  | Many [MatchValue a]
  deriving (Show)

search :: [Pattern a] -> [a] -> Maybe ([a], Match a, [a])
search _ [] = Nothing
search pats (a:as) = case match pats (a:as) of
  Just (m, rest) -> pure ([], m, rest)
  Nothing -> first3 (a:) <$> search pats as
  where
  first3 f (x, y, z) = (f x, y, z)

match :: forall a. [Pattern a] -> [a] -> Maybe (Match a, [a])
match [ManyLeft pat] as = Just $ matchMany pat as
match [ManyRight pat] as = Just $ matchMany pat as
match (ManyLeft pat:pat':pats') as0 = do
  (ms, m, as') <- loop as0
  let ms' = mergeMatches ms `Map.union` m
  first (Map.union ms') <$> match pats' as'
  where
  loop :: [a] -> Maybe ([Match a], Match a, [a])
  loop [] = Nothing
  loop (a:as) = case matchOne pat' a of
    Nothing -> do
      m <- matchOne pat a
      (ms, done, as') <- loop as
      pure (m:ms, done, as')
    Just done -> pure ([], done, as)
match (ManyRight pat:pat':pats') as0 = do
  (ms, m, as') <- loop as0
  let ms' = mergeMatches ms `Map.union` m
  first (Map.union ms') <$> match pats' as'
  where
  loop :: [a] -> Maybe ([Match a], Match a, [a])
  loop [] = Nothing
  loop (a:as) = case (matchOne pat a, matchOne pat' a) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just done) -> pure ([], done, as)
    (Just m, Nothing) -> do
      (ms, done, as') <- loop as
      pure (m:ms, done, as')
    (Just m, Just done) -> case loop as of
      Just (ms, done', as') -> pure (m:ms, done', as')
      Nothing -> pure ([], done, as)
match (pat:pats) (a:as) = do
  m <- matchOne pat a
  first (Map.union m) <$> match pats as
match [] rest = pure (Map.empty, rest)
match (_:_) [] = Nothing

matchOne :: Pattern a -> a -> Maybe (Match a)
matchOne (Var x) a = do
  pure $ Map.singleton x (One a)
matchOne (Sat x_m p) a = do
  let ok = maybe Map.empty (\x -> Map.singleton x (One a)) x_m
  if p a then pure ok else Nothing
matchOne (Rec x_m f pat) a = do
  as <- f a
  let ok = maybe Map.empty (\x -> Map.singleton x (One a)) x_m
  (m, rest) <- match pat as
  case rest of
    [] -> pure $ ok `Map.union` m
    _ -> Nothing
matchOne (ManyLeft pat) a = matchOne pat a
matchOne (ManyRight pat) a = matchOne pat a

matchMany :: forall a. Pattern a -> [a] -> (Match a, [a])
matchMany pat as0 = first mergeMatches $ loop as0
  where
  loop :: [a] -> ([Match a], [a])
  loop [] = ([], [])
  loop (a:as) = case matchOne pat a of
    Nothing -> ([], a:as)
    Just m -> first (m:) $ loop as

mergeMatches :: [Match a] -> Match a
mergeMatches ms = id
  $   Map.map Many
  $   Map.unionsWith (<>)
  $   Map.map (:[])
  <$> ms
