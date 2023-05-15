{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Texpr.PEG.String
  ( runPeg
  , ErrorReport(..)
  , Reason(..)
  ) where

-- import Prelude hiding (fail,sequence)

import Data.List (stripPrefix)
import Data.Map (Map)
import Data.Texpr (Texprs)
import Text.Location.String (Input(..))
import Text.Texpr.PEG.Generic (Stream(..),ErrorReport(..),Reason(..))
import Text.Texpr.Tree (Rule,RuleName,ParamName)

import qualified Data.CharSet as CS
import qualified Text.Location.String as Loc
import qualified Text.Texpr.PEG.Generic as Monad

runPeg ::
     Map RuleName ([ParamName], Rule) -- ^ global rule definitions
  -> Rule -- ^ start rule
  -> Input -- ^ input
  -> Either (ErrorReport Input) (Texprs, Input) -- ^ result with remaining input
runPeg = Monad.runPeg

instance Stream Input where
  location = (.loc)

  takeChar cs inp = case inp.txt of
    c:txt' | c `CS.elem` cs ->
      let loc' = Loc.advance inp.loc [c]
       in Just (c, Input loc' txt')
    _ -> Nothing

  takeChars cs inp =
    let (ok, txt') = span (`CS.elem` cs) inp.txt
        loc' = Loc.advance inp.loc ok
     in (ok, Input loc' txt')

  stripStringPrefix pre inp = do
    txt' <- stripPrefix pre inp.txt
    let loc' = Loc.advance inp.loc pre
    pure $ Input loc' txt'

  takeTexpr = const Nothing

  isAtEnd inp = null $ inp.txt
