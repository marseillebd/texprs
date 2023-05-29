{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | A companion module to "Text.Location" providing operations
--   for 'Position's and 'Text.Location.Range's that index into 'Text's.
module Text.Location.Text
  ( Input(..)
  , startInput
  , advance
  , drop
  , slice
  -- * Source Fragments
  , Source(..)
  , inputToSource
  ) where

import Prelude hiding (drop)

import Data.Function ((&))
import Data.Text (Text)
import Text.Location (Position(..),startPosition,FwdRange,fwd)

import qualified Data.Text as T

-- | Package a 'Text' with the position of its first 'Char'.
-- Useful for maintaining \"remaining input\" state.
data Input = Input
  { loc :: {-# UNPACK #-} !Position
  , txt :: !Text
  }
  deriving (Read,Show)

-- | Create an 'Input' where the string starts at the 'startPosition'.
startInput :: Text -> Input
startInput str = Input startPosition str

-- | Remove characters from the 'Input' until its location
-- matches the given 'Position'.
--
-- If the 'Position' is behind the 'Input', there is no change.
drop :: Input -> Position -> Input
drop inp p' =
  let (_, post) = T.splitAt (p'.nChars - inp.loc.nChars) inp.txt
   in Input p' post

-- | Take characters from the 'Input' bounded by the given 'FwdRange'.
-- Any characters not included in the 'Input' but requested by the 'FwdRange' are not included.
slice :: Input -> FwdRange -> Source
slice inp r = (drop inp r.anchor).txt
  & T.take (r.position.nChars - r.anchor.nChars)
  & Source r

-- TODO generalize to work on multiple kinds of newline
-- | Increases the position according to the input string.
--
-- - 'nChars' in incremented by the length of the string,
-- - 'line' is incremented by the number of @'\n'@ characters
-- -   (resetting 'col' if non-zero), and
-- - 'col' is incremented by the number of characters in the last line.
advance :: Position -> Text -> Position
advance p0 "" = p0
advance p0 str = advHuman . advMachine $ p0
  where
  trailingNl = case T.unsnoc str of  -- because `lines` strips a final newline
    Just ("", '\n') -> []            -- except when the input is just a newline character
    Just (_ , '\n') -> [""]          -- we need to reinstate the trailing newline
    _ -> []
  advMachine p = p{nChars = p.nChars + T.length str}
  advHuman p = case T.lines str <> trailingNl of
    [] -> p
    [""] -> p{col = 1, line = p.line + 1}
    [_] -> p{col = p.col + T.length str}
    ls | l <- last ls -> p{line = p.line + length ls - 1, col = 1 + T.length l}

------------------ Source Fragments ------------------

-- | Package a 'Text' with its starting/ending positions.
-- Useful for saving sections of input text along with their locations,
-- perhaps for later error reporting or debugging information.
data Source = Source
  { loc :: {-# UNPACK #-} !FwdRange
  , txt :: !Text
  }
  deriving (Read,Show)

-- | Create a 'Source' consisting of the entirety of the given 'Input'.
inputToSource :: Input -> Source
inputToSource inp = Source (fwd inp.loc loc') inp.txt
  where loc' = inp.loc `advance` inp.txt
