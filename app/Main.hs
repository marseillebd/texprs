{-# LANGUAGE PatternSynonyms #-}

module Main where

import Text.Texpr.Monad (runPeg,Input(..))
import Text.Texpr.Tree (Rule(..),pattern Alt,pattern Seq)

import Data.CharSet as CS
import Data.Map (Map)

import qualified Data.Map as Map

main :: IO ()
main = do
  print $ runPeg Map.empty (Star $ Seq []) (Input 0 "blagh")
  print $ runPeg Map.empty (Star $ Many CS.empty) (Input 0 "blagh")
  print $ runPeg Map.empty (Star $ Str "") (Input 0 "blagh")
  print $ runPeg Map.empty g1 (Input 0 "Hello, world!")
  print $ runPeg Map.empty g1 (Input 0 "Hello, world")
  print $ runPeg Map.empty g2 (Input 0 "$foo")
  print $ runPeg Map.empty g2 (Input 0 "$foo1")
  print $ runPeg Map.empty g3 (Input 0 "12345")
  print $ runPeg Map.empty g3 (Input 0 "123a45")
  print $ runPeg Map.empty g3 (Input 0 "a45")
  print $ runPeg Map.empty (Fail "uh-oh") (Input 0 "")
  print $ runPeg Map.empty charSetGrammar (Input 0 "[!-[^-~]")
  print $ runPeg Map.empty charSetGrammar (Input 0 "[ \\  abefnrtv \\\\ \\\'\\\" \\[\\-\\]]")
  print $ runPeg Map.empty charSetGrammar (Input 0 "[\\U123ABC]")
  print $ runPeg Map.empty g4 (Input 0 "aaabaaab")
  print $ runPeg Map.empty g4 (Input 0 "aaabaab")
  print $ runPeg Map.empty g5 (Input 0 "hello   world")
  print $ runPeg Map.empty g5 (Input 0 "he110   world")
  print $ runPeg Map.empty g5 (Input 0 "he110   ")
  print $ runPeg Map.empty g5 (Input 0 "   world")
  print $ runPeg Map.empty g5 (Input 0 "helloworld")
  print $ runPeg Map.empty g5 (Input 0 "he110world")
  print $ runPeg Map.empty g5 (Input 0 "hello   world ")
  print $ uncurry runPeg g6 (Input 0 "(1)(23)(456)")
  print $ runPeg Map.empty g7 (Input 0 "123")
  print $ runPeg Map.empty g7 (Input 0 "12")

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
