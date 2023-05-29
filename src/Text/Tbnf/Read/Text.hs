{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Implements readers that take a 'Text' as input.
module Text.Tbnf.Read.Text
  ( runReader
  , Input(..)
  , ReaderError
  , Monad.prior
  , Monad.reason
  , Monad.remaining
  , Reason(..)
  ) where

import Control.Monad (when)
import Data.Texpr (Texprs)
import Text.Location.Text (Input(..))
import Text.Tbnf.Read.Generic (Stream(..),Reason(..))
import Text.Tbnf.Tree (CompiledTbnf)

import qualified Data.CharSet as CS
import qualified Data.Text as T
import qualified Text.Location.Text as Loc
import qualified Text.Tbnf.Read.Generic as Monad

-- | Create a stream of 'Data.Texpr.Texpr's from an input 'Text' that matches
-- the given grammar, or report an error.
runReader ::
     CompiledTbnf
  -> Input -- ^ input
  -> Either ReaderError (Texprs, Input) -- ^ result with remaining input
runReader = Monad.runReader

type ReaderError = Monad.ReaderError Input

instance Stream Input where
  location = (.loc)

  takeChar cs inp = do
    (c, txt') <- T.uncons inp.txt
    when (not $ c `CS.elem` cs) Nothing
    let loc' = Loc.advance inp.loc (T.singleton c)
    pure (c, Input loc' txt')

  takeChars cs inp =
    let (ok, txt') = T.span (`CS.elem` cs) inp.txt
        loc' = Loc.advance inp.loc ok
     in (ok, Input loc' txt')

  stripStringPrefix pre inp = do
    txt' <- T.stripPrefix pre inp.txt
    let loc' = Loc.advance inp.loc pre
    pure $ Input loc' txt'

  takeTexpr = const Nothing

  isAtEnd inp = T.null $ inp.txt
