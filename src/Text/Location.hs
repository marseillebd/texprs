{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

-- | This module exports basic types for reporting and manipulationg locations
-- within plain text documents.
--
-- I primarily envision use in interpreters and compilers,
-- where good reporting of source location is an important tool
-- for human editing of source code.
module Text.Location
  ( -- * Single-point Locations
    Position(..)
  , startPosition
  , advance
  -- ** Rendering
  , tersePos
  , verbosePos
  -- * Contiguous Ranges
  , Range(..)
  , pointRange
  -- ** Forward Ranges
  , FwdRange
  , fwd
  , toFwd
  , fromFwd
  -- * Remaining Input
  , Input(..)
  , startInput
  , drop
  , slice
  -- * Source Fragments
  , Source(..)
  , inputToSource
  ) where

import Prelude hiding (drop)

import Data.Function ((&))
import GHC.Records (HasField(..))

------------------ Positions ------------------

-- | Represents a single point.
-- Conceptually, this is a cursor pointing between two characters.
--
-- The 'line' and 'col' members are meant for human consumption, and so
-- should agree with common text editors.
-- The 'nChars' member is what you want to use to mechanically operate on text
-- (e.g. indexing a string by position, ordering positions, &c).
data Position = Pos
  { nChars :: {-# UNPACK #-} !Int -- ^ number of characters before this point, zero-indexed
  , line :: {-# UNPACK #-} !Int -- ^ line number, one-indexed
  , col :: {-# UNPACK #-} !Int -- ^ column number, one-indexed
  }
  deriving (Read,Show)

startPosition :: Position
startPosition = Pos 0 1 1

instance Eq Position where
  Pos n1 _ _ == Pos n2 _ _ = n1 == n2
instance Ord Position where
  Pos n1 _ _ `compare` Pos n2 _ _ = n1 `compare` n2

tersePos :: Position -> String
tersePos p = concat [ show p.line, ":", show p.col ]

verbosePos :: Position -> String
verbosePos p = concat [ "line ", show p.line, ", column ", show p.col ]

-- TODO move into Text.Location.String
-- TODO generalize to work on multiple kinds of newline
advance :: Position -> String -> Position
advance p0 "" = p0
advance p0 str = advHuman . advMachine $ p0
  where
  trailingNl = case reverse str of  -- because `lines` strips a final newline
    "\n" -> []                      -- except when the input is just a newline character
    '\n':_ -> [""]                  -- we need to reinstate the trailing newline
    _ -> []
  advMachine p = p{nChars = p.nChars + length str}
  advHuman p = case lines str <> trailingNl of
    [] -> p
    [""] -> p{col = 1, line = p.line + 1}
    [_] -> p{col = p.col + length str}
    ls | l <- last ls -> p{line = p.line + length ls - 1, col = 1 + length l}

------------------ Ranges ------------------

-- | This represents a range of characters delimited by anchor and position.
-- This can represent selections in a text editor, or regions of source code
-- that are relevant to an error, and so on.
-- This type makes no particular restrictions on the relative ordering of
-- 'position' and 'anchor'.
--
-- Few operations are provided by default here, as their implementation would
-- vary based on use.
data Range = Range
  { anchor :: {-# UNPACK #-} !Position
  , position :: {-# UNPACK #-} !Position
  }
  deriving (Eq,Ord, Read,Show)

pointRange :: Position -> Range
pointRange p = Range p p

-- | A range where the anchor is always before the position.
newtype FwdRange = Fwd Range
  deriving (Eq,Ord, Read,Show)

instance HasField "anchor" FwdRange Position where getField (Fwd r) = r.anchor
instance HasField "position" FwdRange Position where getField (Fwd r) = r.position

fwd :: Position -> Position -> FwdRange
fwd p1 p2 = Fwd $ if p1 <= p2 then Range p1 p2 else Range p2 p1

toFwd :: Range -> FwdRange
toFwd r = fwd r.anchor r.position

fromFwd :: FwdRange -> Range
fromFwd (Fwd r) = r

instance Semigroup FwdRange where
  a <> b = Fwd $ Range a.anchor b.position


------------------ Remaining Input ------------------

data Input = Input
  { loc :: {-# UNPACK #-} !Position
  , txt :: !String
  }
  deriving (Read,Show)

startInput :: String -> Input
startInput str = Input startPosition str

drop :: Input -> Position -> Input
drop inp p' =
  let (_, post) = splitAt (p'.nChars - inp.loc.nChars) inp.txt
   in Input p' post

-- TODO move into Text.Location.String
slice :: Input -> FwdRange -> Source
slice inp r = (drop inp r.anchor).txt
  & Prelude.take (r.position.nChars - r.anchor.nChars)
  & Source r

------------------ Source Fragments ------------------

data Source = Source
  { loc :: {-# UNPACK #-} !FwdRange
  , txt :: !String
  }
  deriving (Read,Show)

inputToSource :: Input -> Source
inputToSource inp = Source (Fwd $ Range inp.loc loc') inp.txt
  where loc' = inp.loc `advance` inp.txt
