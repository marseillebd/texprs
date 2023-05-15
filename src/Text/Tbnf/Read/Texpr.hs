{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

-- | Implements readers that take a list of 'Data.Texpr.Texpr' as input.
--
-- This is primarily useful as a secondary reader stage.
-- That is, you may find it productive to write a lexer (string to flat
-- (ish) texprs), and then feed the resulting tokens to this module's reader
-- funtions to form a parse tree.
module Text.Tbnf.Read.Texpr
  ( runReader
  , Input(..)
  , ReaderError(..)
  , Reason(..)
  )
  where

import Data.Texpr (Texprs,start,end)
import Text.Location (Position,startPosition)
import Text.Tbnf.Read.Generic (Stream(..),ReaderError(..),Reason(..))
import Text.Tbnf.Tree (CompiledTbnf)

import qualified Text.Tbnf.Read.Generic as Monad

-- | Create a stream of 'Data.Texpr.Texpr's from an input 'Data.Texpr.Texpr'
-- stream that matches the given grammar, or report an error.
runReader ::
     CompiledTbnf
  -> Texprs -- ^ input
  -> Either (ReaderError Input) (Texprs, Input) -- ^ result with remaining input
runReader tbnf ts = Monad.runReader tbnf inp
  where
  inp = Input startPosition ts

-- | The type of input that should be fed to this reader.
data Input = Input
  { loc :: {-# UNPACK #-} !Position -- ^ last position seen before the next token
  , toks :: Texprs -- ^ the token stream
  }

instance Stream Input where
  location = (.loc)

  takeChar _ = const Nothing
  takeChars _ = ("",)
  stripStringPrefix _ = const Nothing

  takeTexpr Input{toks = t:ts} =
    let loc' = case ts of { t':_ -> start t'; _ -> end t }
     in Just (t, Input loc' ts)
  takeTexpr _ = Nothing

  isAtEnd inp = null inp.toks
