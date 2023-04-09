{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Data.List (isPrefixOf,stripPrefix,isSuffixOf)
import Data.Texpr (Texpr(..),Texprs,unparse)
import Text.Location (startInput)
import Text.Texpr.Monad (runPeg)
import Text.Texpr.Tree (Rule(..),pattern Alt,pattern Seq,pattern Star)

import Data.Map (Map)

import qualified Data.CharSet as CS
import qualified Data.Map as Map
import qualified Text.Texpr.Bootstrap as Bs

main :: IO ()
main = main2

------------------------------------

main2 :: IO ()
main2 = do
  selfGrammar <- readFile "docs/autotab.tbnf"
  let r = runPeg Bs.rules Bs.startRule (startInput selfGrammar)
  case r of
    Right (ts, p) -> do
      putStrLn $ concatMap unparse ts
      putStrLn "======"
      print `mapM_` ts
      putStrLn "======"
      print `mapM_` cleanBs ts
      print p
    Left e -> print e

cleanBs :: Texprs -> Texprs
cleanBs ts0 = fmap simpl $ fmap remKw $ concatMap remWs ts0
  where
  remWs t@(Atom _ _) = [t]
  remWs (Combo _ "Space" _) = []
  remWs (Combo _ "Nl" _) = []
  remWs (Combo _ "Comment" _) = []
  remWs (Combo _ "Doc" _) = []
  remWs (Combo l name ts)
    | name == "DEBUG" = [Combo l name ts]
    | "BirdFoot" `isPrefixOf` name = []
    | otherwise = [Combo l name (concatMap remWs ts)]
  remWs t@(Error _ _ _) = [t]
  remKw :: Texpr -> Texpr
  remKw (Combo l "DefClass" (_:name:_:_:body)) = Combo l "DefClass" (name : fmap remKw body)
  remKw (Combo l "ClassVar" [_, name, _]) = Combo l "ClassVar" [name]
  remKw (Combo l "CharRange" [_, c1, _, _, _, c2, _]) = Combo l "CharRange" [c1, c2]
  remKw (Combo l "CharSet" cs) = Combo l "CharSet" $ remKw <$> (init . tail) cs
  remKw (Combo l "Rule.Def" (name : params : _ : rest)) = Combo l "Rule.Def" $ remKw <$> (name:params:rest)
  remKw (Combo l "Rule.Params" xs) = Combo l "Rule.Params" $ evenElems xs
  remKw (Combo l "Rule" (Atom _ "(" : xs)) = Combo l "Rule" $ remKw <$> init xs
  remKw (Combo l "Rule.Group" cs) = Combo l "Rule.Group" $ remKw <$> (init . tail) cs
  remKw (Combo l "Rule.Flat" xs) = Combo l "Rule.Flat" $ remKw <$> (init . tail) xs
  remKw (Combo l "Rule.Call" xs) = Combo l "Rule.Call" $ remKw <$> oddElems xs
  remKw (Combo l "Rule.String" cs) = Combo l "Rule.String" $ remKw <$> (init . tail) cs
  remKw (Combo l "Rule.Sat" (Atom _ x:xs)) = Combo l ctor' $ remKw <$> init xs
    where ctor' = if x == "[^" then "Rule.Sat.Neg" else "Rule.Sat"
  remKw (Combo l name ts)
    | "Char" `isSuffixOf` name
    , [_, c, _] <- ts = Combo l name [remKw c]
    | otherwise = Combo l name (remKw <$> ts)
  remKw t = t
  simpl :: Texpr -> Texpr
  simpl (Combo _ "Rule" [t]) = simpl t
  simpl (Combo l ctor [t])
    | Just ctorRest <- stripPrefix "Rule." ctor
    , ctorRest `elem` ["Term", "Factor", "Group"]
    = simpl t
    | otherwise = Combo l ctor [simpl t]
  simpl t@(Atom _ _) = t
  simpl (Combo l0 "Rule.String" xs0) = Combo l0 "Rule.String" $ loop xs0
    where
    loop [] = []
    loop (Atom l c : xs) = let (l', cs, xs') = loop2 l c xs in Atom (l <> l') cs : loop xs'
    loop (x : xs) = x : loop xs
    loop2 _ acc (Atom l' c : xs) = loop2 l' (acc <> c) xs
    loop2 l acc xs = (l, acc, xs)
  simpl (Combo l ctor xs) = Combo l ctor (simpl <$> xs)
  simpl t = t


evenElems :: [a] -> [a]
evenElems [] = []
evenElems [_] = []
evenElems (_:x:rest) = x : evenElems rest
oddElems :: [a] -> [a]
oddElems [] = []
oddElems [x] = [x]
oddElems (x:_:rest) = x : oddElems rest

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
