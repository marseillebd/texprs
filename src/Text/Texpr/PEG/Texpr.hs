{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Text.Texpr.PEG.Texpr
  ( runPeg
  , Input(..)
  , ErrorReport(..)
  , Reason(..)
  )
  where

import Data.Map (Map)
import Data.Texpr (Texprs,start,end)
import Text.Location (Position,startPosition)
import Text.Texpr.PEG.Generic (Stream(..),ErrorReport(..),Reason(..))
import Text.Texpr.Tree (Rule,RuleName,ParamName)

import qualified Text.Texpr.PEG.Generic as Monad

runPeg ::
     Map RuleName ([ParamName], Rule) -- ^ global rule definitions
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
