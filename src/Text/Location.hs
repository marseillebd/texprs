{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

-- | This module exports basic types for reporting and manipulationg locations
-- within plain text documents.
-- For functions operating on various text formats, see
-- "Text.Location.String",
-- TODO "Text.Location.Text", and
-- TODO "Text.Location.Text.Lazy".
--
-- I primarily envision use in interpreters and compilers,
-- where good reporting of source location is an important tool
-- for human editing of source code.
module Text.Location
  ( -- * Single-point Locations
    Position(..)
  , startPosition
  -- ** Rendering
  , tersePos
  , verbosePos
  -- * Contiguous Ranges
  , Range(..)
  , pointRange
  -- ** Forward Ranges
  , FwdRange
  , fwd
  , maybeFwd
  , toFwd
  , fromFwd
  ) where

import Prelude hiding (drop)

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

-- | The default start position: character zero, line 1, column 1.
startPosition :: Position
startPosition = Pos 0 1 1

instance Eq Position where
  Pos n1 _ _ == Pos n2 _ _ = n1 == n2
instance Ord Position where
  Pos n1 _ _ `compare` Pos n2 _ _ = n1 `compare` n2

-- | Display the position in a compact human-readable format.
-- Specifically, the format is @"\<LINE\>:\<COL\>"@.
--
-- See also 'verbosePos' for an alternate format.
tersePos :: Position -> String
tersePos p = concat [ show p.line, ":", show p.col ]

-- | Display the position in a human-readable format, sparing no characters.
-- Specifically, the format is @"line \<LINE\>, column \<COL\>"@.
--
-- See also 'tersePos' for an alternate format.
verbosePos :: Position -> String
verbosePos p = concat [ "line ", show p.line, ", column ", show p.col ]

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
  -- ^ the position where the range begins chronologically
  -- (i.e. where the cursor began selecting)
  , position :: {-# UNPACK #-} !Position
  -- ^ the position where the range ends chronologically
  -- (i.e. where the cursor stopped selecting)
  }
  deriving (Eq,Ord, Read,Show)

-- | A range at the given position which contains zero characters.
--   Like a non-selection cursor.
pointRange :: Position -> Range
pointRange p = Range p p

-- | A 'Range' where the anchor is always before the position.
newtype FwdRange = Fwd Range
  deriving (Eq,Ord, Read,Show)

instance HasField "anchor" FwdRange Position where getField (Fwd r) = r.anchor
instance HasField "position" FwdRange Position where getField (Fwd r) = r.position

-- | Create a 'FwdRange' from the two input positions,
--   swapping them as necessary to ensure the 'anchor' is before the 'position'.
fwd :: Position -> Position -> FwdRange
{-# INLINE fwd #-}
fwd p1 p2 = Fwd $ if p1 <= p2 then Range p1 p2 else Range p2 p1

maybeFwd :: Position -> Position -> Maybe FwdRange
{-# INLINE maybeFwd #-}
maybeFwd p1 p2 = if p1 <= p2 then Just (Fwd $ Range p1 p2) else Nothing

-- | Convert a 'Range' toa 'FwdRange',
--   swapping them as necessary to ensure the 'anchor' is before the 'position'.
toFwd :: Range -> FwdRange
toFwd r = fwd r.anchor r.position

-- | Convert a 'FwdRange' into a 'Range'
--   without altering the semantics of 'anchor' and 'position'.
fromFwd :: FwdRange -> Range
fromFwd (Fwd r) = r

instance Semigroup FwdRange where
  a <> b = Fwd $ Range a.anchor b.position
