{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Tbnf.Read.Text
  ( Input(..)
  ) where

import Text.Location.Text (Input(..))
import Text.Tbnf.Read.Monad (Stream(..))

instance Stream Input where
