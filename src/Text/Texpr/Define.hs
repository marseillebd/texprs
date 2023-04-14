{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Texpr.Define where

import Data.Char (chr)
import Data.List (partition,isPrefixOf,uncons)
import Data.Texpr (Texpr(..),Texprs)
import Text.Location (FwdRange)

data Peg = Peg
  { start :: StartDef
  , rules :: [RuleDef]
  , classes :: [ClassDef]
  }
  deriving (Show)

parsePeg :: Texprs -> Peg
parsePeg ts0 =
  let ts1 = fmap cleanKeywords $ concatMap cleanSpace ts0
      (classes, ts2) = parseClasses ts1
      (rules, ts3) = parseRules ts2
      start = parseStartDef ts3
   in Peg {start, rules, classes}


data Rule
  = Alt FwdRange [Rule]
  | Seq FwdRange [Rule]
  | Rep FwdRange (Rule, Maybe Rule) (Int, Maybe Int)
  | Sat FwdRange [SatClass]
  | SatNeg FwdRange [SatClass]
  | Char FwdRange Char
  | Str FwdRange String
  | End FwdRange
  | Flat FwdRange Rule
  | Call FwdRange String [Rule]
  | Ctor FwdRange String Rule
  deriving (Show)

-- instance Show Rule where
--   show (Alt _ ts) = concat [ "(Alt " , show ts, ")" ]
--   show (Call _ f args) = concat [ "(Call ", show f, " ", show args, ")" ]
--   show (Rep _ body amt) = concat [ "(Rep ", show body, " ", show amt, ")" ]
--   show (Sat _ clss) = concat [ "(Sat ", show clss, ")" ]
--   show (SatNeg _ clss) = concat [ "(SatNeg ", show clss, ")" ]
--   show (Char _ c) = concat [ "(Char ", show c, ")" ]
--   show (Str _ str) = concat [ "(Str ", show str, ")" ]
--   show (End _) = "End"
--   show (Seq _ ts) = concat [ "(Seq " , show ts, ")" ]
--   show (Flat _ t) = concat [ "(Flat ", show t, ")" ]
--   show (Ctor _ name t) = concat [ "(Ctor ", show name, " ", show t, ")" ]

data SatClass
  = SatVar FwdRange String
  | SatRange FwdRange Char Char
  | SatChar FwdRange Char
  | SatSet FwdRange [Char]
  deriving (Show)

-- instance Show SatClass where
--   show (SatVar _ x) = concat [ "(SatVar ", show x , ")" ]
--   show (SatRange _ lo hi) = concat [ "(SatRange ", show lo, " ", show hi , ")" ]
--   show (SatChar _ c) = concat [ "(SatChar ", show c , ")" ]
--   show (SatSet _ cs) = concat [ "(SatSet ", show cs , ")" ]

data CharClass
  = ClassVar FwdRange String
  | ClassRange FwdRange Char Char
  | ClassChar FwdRange Char
  | ClassSet FwdRange [Char]
  | ClassUnion CharClass CharClass
  | ClassMinus CharClass CharClass
  deriving (Show)

-- instance Show CharClass where
--   show (ClassVar _ str) = concat [ "ClassVar ", show str ]
--   show (ClassRange _ lo hi) = concat ["ClassRange ", show lo, " ", show hi]
--   show (ClassChar _ c) = concat [ "ClassChar ", show c ]
--   show (ClassSet _ cs) = concat [ "ClassSet ", show cs ]
--   show (ClassUnion a b) = concat [ "ClassUnion (", show a, ") (", show b, ")" ]
--   show (ClassMinus a b) = concat [ "ClassMinus (", show a, ") (", show b, ")" ]

-- TODO honestly, all this parsing stuff under here is garbage, I can't wait to replace it using the langs I'm defining

------ Start ------

type StartDef = Maybe (FwdRange, String)

parseStartDef :: Texprs -> StartDef
parseStartDef [] = Nothing
parseStartDef [t] = Just $ go t
  where
  go (Combo l "Start.Def" [Atom _ name]) = (l, name)
  go _ = error "internal Peg grammar error"
parseStartDef _ = error "internal Peg grammar error"

------ Rules ------

type RuleDef = (FwdRange, (FwdRange, String, [String]), Rule)

parseRules :: Texprs -> ([RuleDef], Texprs)
parseRules defs =
  let (rs, defs') = partition isRuleDef defs
      rs' = parseRuleDef <$> rs
   in (rs', defs')
  where
  isRuleDef (Combo _ "Rule.Def" _) = True
  isRuleDef _ = False

parseRuleDef :: Texpr -> RuleDef
parseRuleDef (Combo l "Rule.Def" [binder,body]) = (l, parseRuleBinder binder, parseRule body)
parseRuleDef _ = error "internal Peg grammar error"

parseRuleBinder :: Texpr -> (FwdRange, String, [String])
parseRuleBinder (Atom l str) = (l, str, [])
parseRuleBinder (Combo l "Rule.Parametric" (f:params)) =
  (l, fromName f, fromName <$> params)
  where
  fromName (Atom _ str) = str
  fromName _  = error "internal Peg grammar error"
parseRuleBinder _ = error "internal Peg grammar error"

parseRule :: Texpr -> Rule
parseRule (Combo _ "Rule" [t]) = parseTerm t
parseRule (Combo l "Rule" ts) = Alt l $ loop ts
  where
  loop [t] = [parseTerm t]
  loop (a:Kw "|":rest) = parseTerm a : loop rest
  loop _ = error "internal Peg grammar error"
parseRule _ = error "internal Peg grammar error"

parseTerm :: Texpr -> Rule
parseTerm (Combo _ "Rule.Term" [t]) = parseFactor t
parseTerm (Combo l "Rule.Term" (Combo _ "Rule.Ctor" [Atom ctorL ctor]:ts))
  = Ctor ctorL ctor $ Seq l $ parseFactor <$> ts
parseTerm (Combo l "Rule.Term" ts) = Seq l $ parseFactor <$> ts
parseTerm _ = error "internal Peg grammar error"

parseFactor :: Texpr -> Rule
parseFactor (Combo _ "Rule.Factor" [t0]) = go t0
  where
  go (Combo l "Rule.Rep" [t, amt]) = Rep l (parseCommit t) (parseAmount amt)
  go t = parsePrimRule t
parseFactor _ = error "internal Peg grammar error"

parsePrimRule :: Texpr -> Rule
parsePrimRule (Combo _ "Rule.Group" [t]) = parseRule t
parsePrimRule (Combo l "Rule.Call" (Atom _ f : args)) = Call l f (parseRule <$> args)
parsePrimRule (Combo l "Rule.Char" [c]) = Char l (parseChar c)
parsePrimRule (Combo l "Rule.String" ts) = Str l (concatMap parseStr ts)
parsePrimRule (Combo l "Rule.Sat" ts) = Sat l (parseSatClass <$> ts)
parsePrimRule (Combo l "Rule.Sat.Neg" ts) = SatNeg l (parseSatClass <$> ts)
parsePrimRule (Atom l "$") = End l
parsePrimRule (Combo l "Rule.Flat" [t]) = Flat l (parseRule t)
parsePrimRule _ = error "internal Peg grammar error"

parseSatClass :: Texpr -> SatClass
parseSatClass = \case
  Combo l "Class.Var" [Atom _ name] -> SatVar l name
  Combo l "Char.Range" [lo, hi] -> SatRange l (parseChar lo) (parseChar hi)
  Combo l "Char" [c] -> SatChar l (parseChar c)
  Atom l cs -> SatSet l cs
  _ -> error "internal Peg grammar error"

parseCommit :: Texpr -> (Rule, Maybe Rule)
parseCommit (Combo _ "Rule.Commit" [t1, t2]) = (parseTerm t1, Just $ parseTerm t2)
parseCommit t = (parsePrimRule t, Nothing)

parseAmount :: Texpr -> (Int, Maybe Int)
parseAmount (Kw "*") = (0, Nothing)
parseAmount (Kw "+") = (1, Nothing)
parseAmount (Kw "?") = (0, Just 1)
parseAmount (Combo _ "Rule.Rep.Amount" [Kw ","]) = (0, Nothing)
parseAmount (Combo _ "Rule.Rep.Amount" [t]) = (parseInt t, Just $ parseInt t)
parseAmount (Combo _ "Rule.Rep.Amount" [t,Kw ","]) = (parseInt t, Nothing)
parseAmount (Combo _ "Rule.Rep.Amount" [Kw ",", t]) = (0, Just $ parseInt t)
parseAmount (Combo _ "Rule.Rep.Amount" [t1, Kw ",", t2]) = (parseInt t1, Just $ parseInt t2)
parseAmount _ = error "internal Peg grammar error"

parseInt :: Texpr -> Int
parseInt (Atom _ n) = read n
parseInt _ = error "internal Peg grammar error"

------ Character Classes ------

type ClassDef = (FwdRange, (FwdRange, String), CharClass)

parseClasses :: Texprs -> ([ClassDef], Texprs)
parseClasses defs =
  let (clss, defs') = partition isClassDef defs
      clss' = parseClass <$> clss
   in (clss', defs')
  where
  isClassDef (Combo _ "Class.Def" _) = True
  isClassDef _ = False

parseClass :: Texpr -> (FwdRange, (FwdRange, String), CharClass)
parseClass (Combo l "Class.Def" (name:body)) =
  let parsedName = case name of
        Combo xLoc "Class.Var" [Atom _ x] -> (xLoc, x)
        _ -> error "internal Peg grammar error"
   in (l, parsedName, parseClassBody body)
parseClass _ = error "internal Peg grammar error"

parseClassBody :: Texprs -> CharClass
parseClassBody = loop . reverse
  where
  loop = \case
    [t] -> parseClassTerm t
    (b:o:prior) -> (parseOper o) (loop prior) (parseClassTerm b)
    _ -> error "internal Peg grammar error"
  parseOper (Kw "|") = ClassUnion
  parseOper (Kw "-") = ClassMinus
  parseOper _ = error "internal Peg grammar error"

parseClassTerm :: Texpr -> CharClass
parseClassTerm t = case t of
  Combo l "Class.Var" [Atom _ name] -> ClassVar l name
  Combo l "Char.Range" [lo, hi] -> ClassRange l (parseChar lo) (parseChar hi)
  Combo l "Char" [c] -> ClassChar l (parseChar c)
  Combo l "Char.Set" elems -> ClassSet l (concatMap parseStr elems)
  _ -> error "internal Peg grammar error"

------ Utility Parsers ------

parseChar :: Texpr -> Char
parseChar (Atom _ [c]) = c
parseChar (Combo _ "char.Escape" [Atom _ ['\\',c]]) = case c of
  '0' -> '\NUL'
  'a' -> '\a'
  'b' -> '\b'
  'e' -> '\ESC'
  'f' -> '\f'
  'n' -> '\n'
  'r' -> '\r'
  't' -> '\t'
  'v' -> '\v'
  _ -> error "internal Peg grammar error"
parseChar (Combo _ "char.Escape" [Atom _ kw, Atom _ str]) = case kw of
  "\\x" -> chr $ read ("0x" ++ str)
  "\\u" -> chr $ read ("0x" ++ str)
  "\\U" -> chr $ read ("0x" ++ str)
  "\\" -> head str
  _ -> error "internal Peg grammar error"
parseChar _ = error "internal Peg grammar error"

parseStr :: Texpr -> String
parseStr (Atom _ str) = str
parseStr t@(Combo _ "char.Escape" _) = [parseChar t]
parseStr _ = error "internal Peg grammar error"

------ Texpr Cleaner ------

-- TODO move this compiler into a Text.Texpr.Define.Bootstrap module

cleanSpace :: Texpr -> Texprs
cleanSpace t@(Atom _ _) = [t]
cleanSpace (Combo l name ts)
  | name `elem` ["Space", "Nl", "Comment", "Doc"] = []
  | "BirdFoot" `isPrefixOf` name = []
  | otherwise = [Combo l name (concatMap cleanSpace ts)]
cleanSpace (Error _ _ _) = error "internal Peg error"

cleanKeywords :: Texpr -> Texpr
cleanKeywords t@(Atom _ _) = t
cleanKeywords (Combo l "Rule.Sat" ((cleanKeywords <$>) -> ts))
  | (Kw "[^" : (unsnoc -> Just (ts', Kw "]"))) <- ts
  = Combo l "Rule.Sat.Neg" ts'
cleanKeywords (Combo l ctor ((cleanKeywords <$>) -> ts0)) = Combo l ctor (go ctor ts0)
  where
  go "Class.Def" (name:Kw "=":body) = (name : body)
  go "Class.Var" [Kw ":", name, Kw ":"] = [name]
  go "Char" [Kw "\'", t, Kw "\'"] = [t]
  go "Char.Range" [Kw "\'", t1, Kw "\'", Kw "..", Kw "\'", t2, Kw "\'"] = [t1, t2]
  go "Char.Set" (Kw "\"" : (unsnoc -> Just (ts, Kw "\""))) = ts
  go "Rule.Def" (name : Kw "=" : rest) = name : rest
  go "Rule.Parametric" (f : Kw "<" : (unsnoc -> Just (ts, Kw ">"))) = f : concatMap cleanComma ts
  go "Rule.Group" (Kw "(" : (unsnoc -> Just (ts, Kw ")"))) = ts
  go "Rule.Rep.Amount" (Kw "{" : (unsnoc -> Just (ts, Kw "}"))) = ts
  go "Rule.Commit" [Kw "(", t1, Kw "->", t2, Kw ")"] = [t1, t2]
  go "Rule.Sat" (Kw "[" : (unsnoc -> Just (ts, Kw "]"))) = ts
  go "Rule.Char" (Kw "\'" : (unsnoc -> Just (ts, Kw "\'"))) = ts
  go "Rule.String" (Kw "\"" : (unsnoc -> Just (ts, Kw "\""))) = ts
  go "Rule.Call" (f : Kw "<" : (unsnoc -> Just (ts, Kw ">"))) = f : concatMap cleanComma ts
  go "Rule.Flat" (Kw "/" : (unsnoc -> Just (ts, Kw "/"))) = ts
  go "Rule.Ctor" [name, Kw ":"] = [name]
  go _ other = other
cleanKeywords (Error _ _ _) = error "internal Peg error"

cleanComma :: Texpr -> Texprs
cleanComma (Kw ",") = []
cleanComma t = [t]

pattern Kw :: String -> Texpr
pattern Kw str <- Atom _ str

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = do
  (x,xs') <- uncons (reverse xs)
  pure (reverse xs', x)

cleanGrouping :: Texpr -> Texpr
cleanGrouping t@(Atom _ _)= t
cleanGrouping (Combo l ctor ((cleanGrouping <$>) -> ts)) = case go ctor ts of
  Nothing -> Combo l ctor ts
  Just t' -> t'
  where
  go "Rule" [t] = Just t
  go "Rule.Term" [t] = Just t
  go "Rule.Factor" [t] = Just t
  go "Rule.Group" [t] = Just t
  go _ _ = Nothing
cleanGrouping (Error _ _ _) = error "internal Peg error"
