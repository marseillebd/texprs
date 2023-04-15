{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main,main1,main2) where

import Text.Pretty.Simple

import Data.Map (Map)
import System.Exit (exitFailure)
import Text.Location (startInput)
import Text.Texpr.Monad (runPeg,ErrorReport(..))
import Text.Texpr.Tree (Rule(..),pattern Alt,pattern Seq,pattern Star)

import qualified Data.CharSet as CS
import qualified Data.Map as Map
import qualified Text.Texpr.Bootstrap as Tbnf
import qualified Text.Texpr.Compile as C

main :: IO ()
main = main2

------------------------------------

main2 :: IO ()
main2 = do
  selfGrammar <- readFile "docs/tbnf.tbnf"
  peg1 <- case uncurry runPeg Tbnf.grammar (startInput selfGrammar) of
    Right (ts0, _) -> do
      let ts1 = Tbnf.clean ts0
      let peg = Tbnf.parsePeg ts1
      -- print peg -- NOTE for replacing the bootstrap grammar with the written grammar
      case C.compile peg of
        Right g -> pure g
        Left e -> print e >> exitFailure
    Left e -> do
      print e.prior
      pPrint e.reason
      exitFailure
  peg2 <- case uncurry runPeg peg1 (startInput selfGrammar) of
    Right (ts0, _) -> do
      let ts1 = Tbnf.clean ts0
      let peg = Tbnf.parsePeg ts1
      case C.compile peg of
        Right g -> pure g
        Left e -> print e >> exitFailure
    Left e -> do
      print e.prior
      pPrint e.reason
      exitFailure
  if peg1 == peg2
    then print "^.^"
    else exitFailure

------------------------------------

main1 :: IO ()
main1 = do
  print $ runPeg Map.empty (Star $ Seq []) (startInput "blagh")
  print $ runPeg Map.empty (Star $ Many CS.empty) (startInput "blagh")
  print $ runPeg Map.empty (Star $ Str "") (startInput "blagh")
  print $ runPeg Map.empty g1 (startInput "Hello, world!")
  print $ runPeg Map.empty g1 (startInput "Hello, world")
  print $ runPeg Map.empty g2 (startInput "$foo")
  print $ runPeg Map.empty g2 (startInput "$foo1")
  print $ runPeg Map.empty g3 (startInput "12345")
  print $ runPeg Map.empty g3 (startInput "123a45")
  print $ runPeg Map.empty g3 (startInput "a45")
  -- print $ runPeg Map.empty (Fail "uh-oh") (startInput "")
  print $ runPeg Map.empty charSetGrammar (startInput "[!-[^-~]")
  print $ runPeg Map.empty charSetGrammar (startInput "[ \\  abefnrtv \\\\ \\\'\\\" \\[\\-\\]]")
  print $ runPeg Map.empty charSetGrammar (startInput "[\\U123ABC]")
  print $ runPeg Map.empty g4 (startInput "aaabaaab")
  print $ runPeg Map.empty g4 (startInput "aaabaab")
  print $ runPeg Map.empty g5 (startInput "hello   world")
  print $ runPeg Map.empty g5 (startInput "he110   world")
  print $ runPeg Map.empty g5 (startInput "he110   ")
  print $ runPeg Map.empty g5 (startInput "   world")
  print $ runPeg Map.empty g5 (startInput "helloworld")
  print $ runPeg Map.empty g5 (startInput "he110world")
  print $ runPeg Map.empty g5 (startInput "hello   world ")
  print $ uncurry runPeg g6 (startInput "(1)(23)(456)")
  print $ runPeg Map.empty g7 (startInput "123")
  print $ runPeg Map.empty g7 (startInput "12")

g1 :: Rule
g1 = Seq
  [ Star $ Alt
    [ Str "Hello"
    , Str "world"
    , Str ", "
    ]
  , Str "!"
  ]

g2 :: Rule
g2 = Seq
  [ Many $ CS.oneOf "~!@$%^&*" `CS.union` CS.contiguous 'a' 'z'
  , Alt []
  ]

g3 :: Rule
g3 = Ctor "Int" $ Expect "digits" $ Flat $ Seq [ digit, Star digit ]
  where
  digit = Sat (CS.contiguous '0' '9')

charSetGrammar :: Rule
charSetGrammar = Ctor "Set" $ Seq
  [ Alt [ Str "[^", Str "[" ]
  , Ctor "Contents" $ Star $ Alt [ range, single ]
  , Str "]"
  , Alt []
  ]
  where
  range = Ctor "Range" $ Seq [ single, Str "-", single ]
  single = Alt
    [ Sat $ CS.contiguous '!' '[' `CS.union` CS.contiguous '^' '~'
    , Ctor "Escape" $ Alt
      [ Seq [ Str "\\" , Sat $ CS.oneOf " abefnrtv\\\'\"[-]" ]
      , Seq [ Str "\\x" , Seq (replicate 2 hexDigit) ]
      , Seq [ Str "\\u" , Seq (replicate 4 hexDigit) ]
      , Seq [ Str "\\U" , Seq (replicate 6 hexDigit) ]
      ]
    , Ctor "Ws" $ Flat $ Seq [ wsChar, Star wsChar ]
    ]
  wsChar = Sat $ CS.oneOf " \t"
  hexDigit = Sat $ mconcat
    [ CS.contiguous '0' '9'
    , CS.contiguous 'a' 'f'
    , CS.contiguous 'A' 'F'
    ]

g4 :: Rule
g4 = Capture "foo" (Seq [Star (Str "a"), Str "b"]) $ Replay "foo"

g5 :: Rule
g5 = Seq [ Recover ident space, Recover ident eof ]
  where
  ident = Ctor "Ident" $ Flat $ plus $ Sat $ CS.contiguous 'a' 'z'
  space = Ctor "Ws" $ Flat $ plus $ Sat $ CS.oneOf " \t"
  plus g = Seq [g, Star g]
  eof = Expect "eof" $ Alt []

g6 :: (Map String ([String], Rule), Rule)
g6 =
  ( Map.fromList
    [ ("S", ([], line))
    , ("parens", (["inner"], parens))
    , ("num", ([], num))
    ]
  , line
  )
  where
  line = Star (Call "parens" [Call "num" []]) `Recover` Void
  parens = Ctor "Number" $
              Str "("
       `Seq2` Call "inner" []
    `Recover` Str ")"
  num = Flat $ Sat digit `Seq2` Many digit
  digit = CS.contiguous '0' '9'

g7 :: Rule
g7 = Expect "ThreeDigits" $ Seq [digit, digit, digit]
  where
  digit = Sat $ CS.contiguous '0' '9'
