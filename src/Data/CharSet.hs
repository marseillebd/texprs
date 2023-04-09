{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Data.CharSet
  ( CharSet
  -- construction
  , empty
  , any
  , singleton
  , oneOf
  , contiguous
  , fromFunction
  -- combination
  , union
  , complement
  , minus
  -- query
  , elem
  -- inspection
  , null
  -- conversion
  , render
  ) where

import Prelude hiding (any,elem,null)

import Data.Char (chr,ord)
import Data.List (intercalate)

import qualified Prelude

data CharRange = CR
  { lo :: {-# UNPACK #-} !Char
  , hi :: {-# UNPACK #-} !Char
  }
  deriving (Eq,Ord,Show)
newtype CharSet = CS [CharRange]
  deriving (Eq,Ord,Show)

empty :: CharSet
empty = CS []

any :: CharSet
any = CS [CR minBound maxBound]

singleton :: Char -> CharSet
singleton c = CS [CR c c]

oneOf :: [Char] -> CharSet
oneOf = foldl union empty . fmap singleton

contiguous :: Char -> Char -> CharSet
contiguous a b
  | a <= b = CS [CR a b]
  | otherwise = empty

fromFunction :: (Char -> Bool) -> CharSet
{-# NOINLINE fromFunction #-}
fromFunction f = CS $ loop [minBound .. maxBound]
  where
  f' c = if c == maxBound then False else f (succ c)
  loop rest = case span f' $ dropWhile (not . f) rest of
    ([], []) -> []
    ([], b:rest') -> CR b b : loop rest'
    (a:_, b:rest') -> CR a b : loop rest'
    (_:_, []) -> errorWithoutStackTrace "found matching chars with no final matching char"

union :: CharSet -> CharSet -> CharSet
union (CS a0) (CS b0) = CS $ merge a0 b0
  where
  merge [] [] = []
  merge a [] = a
  merge [] b = b
  merge (rA : a) (rB : b)
    -- ranges are tangent
    | chr (ord rA.hi + 1) == rB.lo = CR rA.lo rB.hi : merge a b
    | chr (ord rB.hi + 1) == rA.lo = CR rB.lo rA.hi : merge a b
    -- range A is strictly less than range B
    | rA.hi < rB.lo = rA : merge a (rB : b)
    -- range B is strictly less than range A
    | rB.hi < rA.lo = rB : merge (rA : a) b
    -- ranges overlap
    | otherwise = CR (min rA.lo rB.lo) (max rA.hi rB.hi) : merge a b

complement :: CharSet -> CharSet
complement cs = any `minus` cs

minus :: CharSet -> CharSet -> CharSet
minus (CS a0) (CS b0) = CS $ merge a0 b0
  where
  merge [] [] = []
  merge a [] = a
  merge [] _ = []
  merge (rA : a) (rB : b)
    -- range A is strictly less than range B
    | rA.hi < rB.lo = rA : merge a (rB : b)
    -- range B is strictly less than range A
    | rB.hi < rA.lo = merge (rA : a) b
    -- ranges overlap
    | otherwise = merge (sub rA rB <> a) (rB : b)
  sub a b
    -- normalize B so that is does not extend outside a
    | b.lo < a.lo = sub a b{lo = a.lo}
    | a.hi < b.hi = sub a b{hi = a.hi}
    -- A is a subset of B
    | a == b = []
    -- B covers a lower part of A
    | a.lo == b.lo && b.hi < a.hi = [CR (chr $ ord b.hi + 1) a.hi]
    -- B covers a upper part of A
    | a.lo < b.lo && b.hi == a.hi = [CR a.lo (chr $ ord b.lo - 1)]
    -- B covers a middle part of A
    | otherwise =
      [ CR a.lo (chr $ ord b.lo - 1)
      , CR (chr $ ord b.hi + 1) a.hi
      ]


elem :: Char -> CharSet -> Bool
elem c (CS rs0) = loop rs0
  where
  loop [] = False
  loop (r : rs)
    | r.lo <= c && c <= r.hi = True
    | otherwise = loop rs

null :: CharSet -> Bool
null (CS rs) = Prelude.null rs

render :: CharSet -> String
render (CS rs) = intercalate "," (renderRange <$> rs)
  where
  renderRange r
    | r.lo == r.hi = r.lo:""
    | otherwise = concat [r.lo:"", "-", r.hi:""]

instance Semigroup CharSet where (<>) = union
instance Monoid CharSet where mempty = empty
