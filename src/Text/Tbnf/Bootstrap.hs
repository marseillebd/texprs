{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module defines the grammar of TBNF grammars.
-- With this, any other TBNF grammar can be defined in its own file, and then
-- read in, parsed, and compiled.
--
-- See @docs/tbnf.tbnf@ in the source tarball for the definition and
-- documentation of TBNF grammars.
module Text.Tbnf.Bootstrap
  ( tbnf
  , grammar
  , clean
  , parseTbnf
  ) where

import Data.Char (chr)
import Data.List (partition,uncons)
import Data.String (fromString)
import Data.Texpr (Texpr(..),Texprs)
import Data.Text (Text)
import Text.Location (FwdRange)
import Text.Tbnf (compile,CompiledTbnf,RuleName,ParamName)
import Text.Tbnf (Tbnf(..),Rule(..),SatClass(..),CharClass(..))
import Text.Tbnf.Bootstrap.Grammar (tbnf)

import qualified Data.Text as T

------ Base Grammar  ------

-- | A compiled form of 'tbnf'.
grammar :: CompiledTbnf
grammar = case compile tbnf of
  Right g -> g
  Left e -> errorWithoutStackTrace $ "internal Tbnf error: " ++ show e

------ Cleanup ------

pattern Kw :: Text -> Texpr
pattern Kw str <- Atom _ str

-- | Eliminates unnecessary nodes (whitespace, keywords, &c) from a TBNF parse
-- tree.
clean :: Texprs -> Texprs
clean ts = cleanKeywords <$> concatMap cleanSpace ts

cleanSpace :: Texpr -> Texprs
cleanSpace t@(Atom _ _) = [t]
cleanSpace (Combo l name ts)
  | name `elem` ["space", "nl", "comment", "doc", "bird-foot"] = []
  | otherwise = [Combo l name (concatMap cleanSpace ts)]

cleanKeywords :: Texpr -> Texpr
cleanKeywords t@(Atom _ _) = t
cleanKeywords (Combo l ctor ((cleanKeywords <$>) -> ts0)) = Combo l ctor (go ctor ts0)
  where
  go "class-char" [Kw "\'", c, Kw "\'"] = [c]
  go "class-range" [Kw "\'", lo, Kw "\'", Kw "..", Kw "\'", hi, Kw "\'"] = [lo, hi]
  go "class-set" (Kw "\"" : (unsnoc -> Just (ts, Kw "\""))) = ts
  go "class-var" [Kw ":", name, Kw ":"] = [name]
  go "def-class" (Kw ":":name:Kw ":":Kw "=":body) = (name : body)
  go "def-rule" (name : Kw "=" : rest) = name : rest
  go "rep-custom" (Kw "{" : (unsnoc -> Just (ts, Kw "}"))) = ts
  go "rep-custom" [Kw "{", comma@(Kw ","), hi, Kw "}"] = [comma, hi]
  go "rep-custom" [Kw "{", lo, comma@(Kw ","), hi, Kw "}"] = [lo, comma, hi]
  go "rep-custom" [Kw "{", lo, comma@(Kw ","), Kw "}"] = [lo, comma]
  go "rule-call" (f : Kw "<" : (unsnoc -> Just (ts, Kw ">"))) = f : concatMap cleanComma ts
  go "rule-capture" [Kw "@", name, Kw "=", capture, scope] = [name, capture, scope]
  go "rule-char" [Kw "\'", c, Kw "\'"] = [c]
  go "rule-combo" [Kw "{", name , Kw "}"] = [name]
  go "rule-commit" [Kw "(", t1, Kw "->", t2, Kw ")"] = [t1, t2]
  go "rule-ctor" (name : Kw ":" : rest) = name:rest
  go "rule-empty" [Kw "1"] = []
  go "rule-end" [Kw "$"] = []
  go "rule-expect" (rule : Kw "??" : Kw "\"" : (unsnoc -> Just (msg, Kw "\""))) = (rule : msg)
  go "rule-flat" (Kw "/" : (unsnoc -> Just (ts, Kw "/"))) = ts
  go "rule-lookahead-neg" [Kw "?!", Kw "\"", msg, Kw "\"", t] = [msg, t]
  go "rule-lookahead-pos" [Kw "?=", t] = [t]
  go "rule-group" (Kw "(" : (unsnoc -> Just (ts, Kw ")"))) = ts
  go "rule-parametric" (f : Kw "<" : (unsnoc -> Just (ts, Kw ">"))) = f : concatMap cleanComma ts
  go "rule-sat" [Kw "[", pos, Kw "^", neg, Kw "]"] = [pos, neg]
  go "rule-sat" [Kw "[", pos, Kw "]"] = [pos]
  go "rule-sat" [Kw "[", Kw "^", neg, Kw "]"] = [neg]
  go "rule-sat" (Kw "[" : (unsnoc -> Just (ts, Kw "]"))) = ts
  go "rule-string" (Kw "\"" : (unsnoc -> Just (ts, Kw "\""))) = ts
  go "rule-void" (Kw "0" : Kw ":" : Kw "\"" : (unsnoc -> Just (ts, Kw "\""))) = ts
  go "sat-char" [Kw "\'", c, Kw "\'"] = [c]
  go "sat-range" [Kw "\'", lo, Kw "\'", Kw "-", Kw "\'", hi, Kw "\'"] = [lo, hi]
  go "sat-range" [Kw "\'", lo, Kw "\'", Kw "-", hi] = [lo, hi]
  go "sat-range" [lo, Kw "-", Kw "\'", hi, Kw "\'"] = [lo, hi]
  go "sat-range" [lo, Kw "-", hi] = [lo, hi]
  go "sat-var" [Kw ":", name, Kw ":"] = [name]
  go _ other = other

cleanComma :: Texpr -> Texprs
cleanComma (Kw ",") = []
cleanComma t = [t]

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = do
  (x,xs') <- uncons (reverse xs)
  pure (reverse xs', x)

-- | Translate the given the ('clean'ed) parse tree of a TBNF grammar
-- into an abstract syntax tree.
parseTbnf :: Texprs -> Tbnf
parseTbnf ts0 =
  let (classes, ts1) = parseClasses ts0
      (rules, ts2) = parseRules ts1
      start = parseStartDef ts2
   in Tbnf {start, rules, classes}

------ Start ------

type StartDef = Maybe (FwdRange, RuleName)

parseStartDef :: Texprs -> StartDef
parseStartDef [] = Nothing
parseStartDef [t] = Just $ go t
  where
  go (Combo l "def-start" [Atom _ name]) = (l, fromString $ T.unpack name)
  go _ = error "internal Tbnf grammar error"
parseStartDef _ = error "internal Tbnf grammar error"

------ Rules ------

type RuleDef = (FwdRange, (FwdRange, RuleName, [ParamName]), Rule)

parseRules :: Texprs -> ([RuleDef], Texprs)
parseRules defs =
  let (rs, defs') = partition isRuleDef defs
      rs' = parseRuleDef <$> rs
   in (rs', defs')
  where
  isRuleDef (Combo _ "def-rule" _) = True
  isRuleDef _ = False

parseRuleDef :: Texpr -> RuleDef
parseRuleDef (Combo l "def-rule" [binder,body]) = (l, parseRuleBinder binder, parseRule body)
parseRuleDef _ = error "internal Tbnf grammar error"

parseRuleBinder :: Texpr -> (FwdRange, RuleName, [ParamName])
parseRuleBinder (Atom l str) = (l, fromString $ T.unpack str, [])
parseRuleBinder (Combo l "rule-parametric" (f:params)) =
  (l, fromName f, fromName <$> params)
  where
  fromName (Atom _ str) = fromString $ T.unpack str
  fromName _  = error "internal Tbnf grammar error"
parseRuleBinder _ = error "internal Tbnf grammar error"

parseRule :: Texpr -> Rule
parseRule (Combo l "rule-alt" ts) = Alt l $ loop ts
  where
  loop [t] = [parseRule t]
  loop (a:Kw "|":rest) = parseRule a : loop rest
  loop _ = error "internal Tbnf grammar error"
parseRule (Combo l "rule-call" (Atom _ f : args)) = Call l (fromString $ T.unpack f) (parseRule <$> args)
parseRule (Combo l "rule-capture" [(Atom _ name), capture, scope])
    = Cap l (fromString $ T.unpack name) (parseRule capture) (parseRule scope)
parseRule (Combo l "rule-char" [c]) = Char l $ parseChar c
parseRule (Combo _ "rule-combo" [Atom l ctor]) = TexprCombo l (fromString $ T.unpack ctor)
parseRule (Combo _ "rule-ctor" [Atom l ctor, g]) = Ctor l (fromString $ T.unpack ctor) $ parseRule g
parseRule (Combo l "rule-empty" []) = Empty l
parseRule (Combo l "rule-end" []) = End l
parseRule (Combo l "rule-expect" (g : msg)) = Expect l (parseRule g) (T.pack $ concatMap parseStr msg)
parseRule (Combo l "rule-flat" [g]) = Flat l $ parseRule g
parseRule (Combo _ "rule-group" [g]) = parseRule g
parseRule (Combo l "rule-lookahead-neg" [Combo _ "msg" msg, g])
  = NegLookahead l (T.pack $ concatMap parseStr msg) (parseRule g)
parseRule (Combo l "rule-lookahead-pos" [g]) = Lookahead l $ parseRule g
parseRule (Combo l "rule-rep" [g, amt]) = Rep l (parseRule g) (parseAmount amt)
  where
  parseAmount :: Texpr -> (Int, Maybe Int)
  parseAmount (Kw "*") = (0, Nothing)
  parseAmount (Kw "+") = (1, Nothing)
  parseAmount (Kw "?") = (0, Just 1)
  parseAmount (Combo _ "rep-custom" [Kw ","]) = (0, Nothing)
  parseAmount (Combo _ "rep-custom" [t]) = (parseInt t, Just $ parseInt t)
  parseAmount (Combo _ "rep-custom" [t,Kw ","]) = (parseInt t, Nothing)
  parseAmount (Combo _ "rep-custom" [Kw ",", t]) = (0, Just $ parseInt t)
  parseAmount (Combo _ "rep-custom" [t1, Kw ",", t2]) = (parseInt t1, Just $ parseInt t2)
  parseAmount _ = error "internal Tbnf grammar error"
  parseInt :: Texpr -> Int
  parseInt (Atom _ n) = read (T.unpack n)
  parseInt _ = error "internal Tbnf grammar error"
parseRule (Combo l "rule-sat" [Combo _ "pos" pos]) = Sat l (Just $ parseSatClass <$> pos) []
parseRule (Combo l "rule-sat" [Combo _ "neg" neg]) = Sat l Nothing (parseSatClass <$> neg)
parseRule (Combo l "rule-sat" [Combo _ "pos" [], Combo _ "neg" neg]) = Sat l Nothing (parseSatClass <$> neg)
parseRule (Combo l "rule-sat" [Combo _ "pos" pos, Combo _ "neg" neg]) = Sat l (Just $ parseSatClass <$> pos) (parseSatClass <$> neg)
parseRule (Combo l "rule-sat" ts) = Sat l (Just $ parseSatClass <$> ts) []
parseRule (Combo l "rule-seq" ts) = Seq l $ parseRule <$> ts
parseRule (Combo l "rule-string" ts) = Str l (T.pack $ concatMap parseStr ts)
parseRule (Combo l "rule-void" [Atom _ msg]) = Void l msg
parseRule t = error $ "internal Tbnf grammar error\n  " ++ show t

parseSatClass :: Texpr -> SatClass
parseSatClass = \case
  Combo l "sat-char" [c] -> SatChar l (parseChar c)
  Combo l "sat-range" [lo, hi] -> SatRange l (parseChar lo) (parseChar hi)
  Combo l "sat-var" [Atom _ name] -> SatVar l (fromString $ T.unpack name)
  Atom l cs -> SatSet l (T.unpack cs)
  t -> error $ "internal Tbnf grammar error\n  " ++ show t ++ "\n  "

------ Character Classes ------

type ClassDef = (FwdRange, (FwdRange, RuleName), CharClass)

parseClasses :: Texprs -> ([ClassDef], Texprs)
parseClasses defs =
  let (clss, defs') = partition isClassDef defs
      clss' = parseClass <$> clss
   in (clss', defs')
  where
  isClassDef (Combo _ "def-class" _) = True
  isClassDef _ = False

parseClass :: Texpr -> (FwdRange, (FwdRange, RuleName), CharClass)
parseClass (Combo l "def-class" (Atom xLoc (T.unpack -> x) : body)) =
  (l, (xLoc, fromString x), parseClassBody body)
parseClass _ = error "internal Tbnf grammar error"

parseClassBody :: Texprs -> CharClass
parseClassBody = loop . reverse
  where
  loop = \case
    [t] -> parseClassTerm t
    (b:o:prior) -> (parseOper o) (loop prior) (parseClassTerm b)
    _ -> error "internal Tbnf grammar error"
  parseOper (Kw "|") = ClassUnion
  parseOper (Kw "-") = ClassMinus
  parseOper _ = error "internal Tbnf grammar error"

parseClassTerm :: Texpr -> CharClass
parseClassTerm t = case t of
  Combo l "class-var" [Atom _ (T.unpack -> name)] -> ClassVar l (fromString name)
  Combo l "class-char" [c] -> ClassChar l (parseChar c)
  Combo l "class-range" [lo, hi] -> ClassRange l (parseChar lo) (parseChar hi)
  Combo l "class-set" elems -> ClassSet l (concatMap parseStr elems)
  _ -> error "internal Tbnf grammar error"

------ Utility Parsers ------

parseChar :: Texpr -> Char
parseChar (Atom _ (T.unpack -> [c])) = c
parseChar (Combo _ "char-escape" [Atom _ (T.unpack -> ['\\',c])]) = case c of
  '0' -> '\NUL'
  'a' -> '\a'
  'b' -> '\b'
  'e' -> '\ESC'
  'f' -> '\f'
  'n' -> '\n'
  'r' -> '\r'
  't' -> '\t'
  'v' -> '\v'
  _ -> error "internal Tbnf grammar error"
parseChar (Combo _ "char-escape" [Atom _ kw, Atom _ (T.unpack -> str)]) = case kw of
  "\\x" -> chr $ read ("0x" ++ str)
  "\\u" -> chr $ read ("0x" ++ str)
  "\\U" -> chr $ read ("0x" ++ str)
  "\\" -> head str
  _ -> error "internal Tbnf grammar error"
parseChar t = error $ "internal Tbnf grammar error: parseChar " ++ show t

parseStr :: Texpr -> String
parseStr (Atom _ str) = T.unpack str
parseStr t@(Combo _ "char-escape" _) = [parseChar t]
parseStr _ = error "internal Tbnf grammar error"
