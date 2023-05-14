{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Text.Texpr.Monad.Texpr
  ( runPeg
  , ErrorReport(..)
  , Reason(..)
  )
  where

import Data.Map (Map)
import Data.Texpr (Texprs,start,end)
import Text.Location (Position,startPosition)
import Text.Texpr.Monad.Generic (Stream(..),ErrorReport(..),Reason(..))
import Text.Texpr.Tree (Rule)

import qualified Text.Texpr.Monad.Generic as Monad

runPeg ::
     Map String ([String], Rule) -- ^ global rule definitions
  -> Rule -- ^ start rule
  -> Texprs -- ^ input
  -> Either (ErrorReport Input) (Texprs, Input) -- ^ result with remaining input
runPeg rules startRule ts = Monad.runPeg rules startRule inp
  where
  inp = Input startPosition ts

data Input = Input
  { loc :: {-# UNPACK #-} !Position
  , toks :: Texprs
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
