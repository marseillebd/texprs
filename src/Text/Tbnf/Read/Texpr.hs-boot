module Text.Tbnf.Read.Texpr
  ( Input(..)
  , ReaderError(..)
  )
  where

import Data.Texpr (Texprs)
import Text.Location (Position)
import Text.Tbnf.Read.Monad (ReaderError(..),Stream)

data Input = Input
  { loc :: {-# UNPACK #-} !Position
  , toks :: Texprs
  }

instance Stream Input where
