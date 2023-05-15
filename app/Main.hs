{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Text.Pretty.Simple

import System.Exit (exitFailure)
import Text.Location.String (startInput)
import Text.Tbnf.Read.String (runReader,ReaderError(..))

import qualified Text.Tbnf as Tbnf
import qualified Text.Tbnf.Bootstrap as Tbnf

main :: IO ()
main = main2

------------------------------------

main2 :: IO ()
main2 = do
  selfGrammar <- readFile "docs/tbnf.tbnf"
  peg1 <- case runReader Tbnf.grammar (startInput selfGrammar) of
    Right (ts0, _) -> do
      let ts1 = Tbnf.clean ts0
      let peg = Tbnf.parseTbnf ts1
      -- print peg -- NOTE for replacing the bootstrap grammar with the written grammar
      case Tbnf.compile peg of
        Right g -> pure g
        Left e -> print e >> exitFailure
    Left e -> do
      print e.prior
      pPrint e.reason
      exitFailure
  peg2 <- case runReader peg1 (startInput selfGrammar) of
    Right (ts0, _) -> do
      let ts1 = Tbnf.clean ts0
      let peg = Tbnf.parseTbnf ts1
      case Tbnf.compile peg of
        Right g -> pure g
        Left e -> print e >> exitFailure
    Left e -> do
      print e.prior
      pPrint e.reason
      exitFailure
  if peg1 == peg2
    then print ("^.^" :: String)
    else exitFailure
