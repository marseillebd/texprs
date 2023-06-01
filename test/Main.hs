{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Text.Pretty.Simple

import Data.Maybe (fromJust)
import Data.Tree.Match (Pattern)
import Data.Tree.Template (Template)
import System.Exit (exitFailure)
import Text.Location.String (startInput)
import Text.Tbnf.Read.String (runReader,prior,reason)

import qualified Data.Tree.Match as Match
import qualified Data.Tree.Template as Template
import qualified Text.Tbnf as Tbnf
import qualified Text.Tbnf.Bootstrap as Tbnf

main :: IO ()
main = _main2

------------------------------------

_main2 :: IO ()
_main2 = do
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
      print (prior e)
      pPrint (reason e)
      exitFailure
  if peg1 == peg2
    then print ("^.^" :: String)
    else putStrLn "Parse from bootstrap and grammar file non-identical" >> exitFailure


------------------------------------

data Tree a = N [Tree a] | L a
  deriving (Eq,Show)

saplings :: Tree a -> Maybe [Tree a]
saplings (N xs) = Just xs
saplings (L _) = Nothing

_main3 :: IO ()
_main3 = do
  let trees = [L 1, L 0, L 0, L 2, N [L 5, L 0, L 3], L 0, L 4, L 5, L 0] :: [Tree Int]
      pat = [Match.Sat Nothing (==L 0)]
      tmpl = []
  let pat' = [Match.Rec Nothing saplings [Match.Var "x", Match.Var "y"]]
      tmpl' = [Template.Combo N [Template.Var "y", Template.Var "x"]]
  let match = fromJust $ Match.search pat trees
      trees' = rewriteTD pat tmpl trees
      trees'' = rewriteTD pat' tmpl' trees'
  print trees
  print match
  print trees'
  print trees''

rewriteTD :: [Pattern (Tree a)]  -> [Template (Tree a)] -> [Tree a] -> [Tree a]
rewriteTD pat tmpl = loop . rewriteLR pat tmpl
  where
  loopOuter = rewriteTD pat tmpl
  loop [] = []
  loop (L x:inp') = L x : loop inp'
  loop (N xs:inp') = N (loopOuter xs) : loop inp'

rewriteLR :: [Pattern a] -> [Template a] -> [a] -> [a]
rewriteLR pat tmpl inp0 = loop inp0
  where
  loop inp = case Match.search pat inp of
    Nothing -> inp
    Just (pre, m, post) -> case Template.rewrite tmpl m of
      Nothing -> inp0
      Just inner -> pre <> inner <> loop post
