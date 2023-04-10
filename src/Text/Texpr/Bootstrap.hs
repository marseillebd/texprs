{-# LANGUAGE PatternSynonyms #-}

module Text.Texpr.Bootstrap where

import Data.CharSet (CharSet)
import Data.Map (Map)
import Text.Texpr.Tree (Rule(..), pattern Seq, pattern Alt, pattern Star, pattern Star1)

import qualified Data.CharSet as CS
import qualified Data.Map as Map

------ Rule Set ------

startRule :: Rule
startRule = Seq -- TODO
  [ Star $ Alt
    [ Call "Doc" []
    , Seq
      [ Call "BirdFoot" []
      , Alt
        [ Call "Rule.Def" []
        , Call "Class.Def" []
        ]
      , opt $ Call "ws" []
      , Alt [ Call "Nl" [], End ]
      ]
    , Call "blank" []
    ]
  , End
  ]

rules :: Map String ([String], Rule)
rules = Map.fromList
  [ ("Doc", ([], doc))
  , ("Doc.line", ([], doc_line_))
  , ("Rule.Def", ([], rule_def))
  , ("Rule.name", ([], rule_name_))
  , ("Rule.name.lower", ([], rule_name_lower_))
  , ("Rule.Parametric", ([], rule_parametric))
  , ("Rule.Group", ([], rule_group))
  , ("Rule", (["flat.in"], rule))
  , ("Rule.Term", (["flat.in"], rule_term))
  , ("Rule.Factor", (["flat.in"], rule_factor))
  , ("Rule.Rep", (["flat.in"], rule_rep))
  , ("Rule.Rep.Amount", ([], rule_rep_range))
  , ("Rule.Commit", ([], rule_commit))
  , ("Rule.prim", (["flat.in"], rule_prim_))
  , ("Rule.Sat", ([], rule_sat))
  , ("Rule.Char", ([], rule_char))
  , ("Rule.String", ([], rule_string))
  , ("Rule.Call", ([], rule_call))
  , ("Rule.Flat", ([], rule_flat))
  , ("Class.Def", ([], classDef))
  , ("Class.body", ([], class_body_))
  , ("Class.Var", ([], class_var))
  , ("Class.name", ([], class_name_))
  , ("Class.term", ([], class_term_))
  , ("Class.operator", ([], class_operator_))
  , ("Char", ([], char))
  , ("Char.Range", ([], char_range))
  , ("Char.Set", ([], char_set))
  , ("char.sq", ([], char_sq_))
  , ("chars.dq", ([], chars_dq_))
  , ("char.Escape", ([], char_escape))
  , ("num.nat", ([], num_nat_))
  , ("Space", ([], space))
  , ("Nl", ([], nl))
  , ("BirdFoot", ([], birdFoot))
  , ("BirdFoot.Improper", ([], birdFoot_improper))
  , ("Comment", ([], comment))
  , ("sep.by", sep_by_)
  , ("sep.dot", sep_dot_)
  , ("ws", ([], ws_))
  , ("blank", ([], blank_))
  ]

------ Literate Documentation ------

doc :: Rule
doc = Ctor "Doc" $ plus $ Alt
  [ Seq [ Call "Doc.line" [], opt $ Call "Nl" [] ]
  , Call "Nl" []
  ]
doc_line_ :: Rule
doc_line_ = Flat $ Seq
  [ Sat $ CS.complement $ CS.oneOf ">\n"
  , Many $ CS.complement $ CS.singleton '\n'
  ]

------ Defining Rules ------

rule_def :: Rule
rule_def = Ctor "Rule.Def" $ Seq
  [ Alt
    [ Call "Rule.Parametric" []
    , Call "Rule.name" []
    ]
  , Call "ws" []
  , Str "="
  , Call "ws" []
  , Call "Rule" [Call "Rule.Flat" []]
  ]

rule_parametric :: Rule
rule_parametric = Ctor "Rule.Parametric" $ Seq
  [ Call "Rule.name" []
  , Str "<"
  , opt $ Call "ws" []
  , Call "sep.by"
    [ Seq [ Str ",", opt $ Call "ws" [] ]
    , Call "Rule.name.lower" []
    ]
  , opt $ Call "ws" []
  , Str ">"
  ]

rule :: Rule
rule = Ctor "Rule" $ Seq
  [ Call "Rule.Term" [Call "flat.in" []]
  , Star2 (Call "ws" []) $ Seq
    [ Str "|"
    , Call "ws" []
    , Call "Rule.Term" [Call "flat.in" []]
    ]
  ]

rule_term :: Rule
rule_term = Ctor "Rule.Term" $ Seq
  [ Call "Rule.Factor" [Call "flat.in" []]
  , Star2 (Call "ws" []) (Call "Rule.Factor" [Call "flat.in" []])
  ]

rule_factor :: Rule
rule_factor = Ctor "Rule.Factor" $ Alt
  [ Call "Rule.Rep" [Call "flat.in" []]
  , Call "Rule.prim" [Call "flat.in" []]
  ]

rule_rep :: Rule
rule_rep = Ctor "Rule.Rep" $ Alt
  [ Seq
    [ Call "Rule.Commit" []
    , Sat (CS.oneOf "*+") `Alt2` Call "Rule.Rep.Amount" []
    ]
  , Seq
    [ Call "Rule.prim" [Call "flat.in" []]
    , Sat (CS.oneOf "*+?") `Alt2` Call "Rule.Rep.Amount" []
    ]
  ]

rule_rep_range :: Rule
rule_rep_range = Ctor "Rule.Rep.Amount" $ Alt
  [ Seq
    [ Str "{"
    , opt $ Call "ws" []
    , opt $ Call "num.nat" []
    , opt $ Call "ws" []
    , Str ","
    , opt $ Call "ws" []
    , opt $ Call "num.nat" []
    , opt $ Call "ws" []
    , Str "}"
    ]
  , Seq
    [ Str "{"
    , opt $ Call "ws" []
    , Call "num.nat" []
    , opt $ Call "ws" []
    , Str "}"
    ]
  ]

rule_commit :: Rule
rule_commit = Ctor "Rule.Commit" $ Seq
  [ Str "("
  , opt $ Call "ws" []
  , Call "Rule.Term" [Call "Rule.Flat" []]
  , Call "ws" []
  , Str "->"
  , Call "ws" []
  , Call "Rule.Term" [Call "Rule.Flat" []]
  , opt $ Call "ws" []
  , Str ")"
  ]

rule_prim_ :: Rule
rule_prim_ = Alt
  [ Call "Rule.Group" []
  , Call "Rule.Call" []
  , Call "Rule.Char" []
  , Call "Rule.String" []
  , Call "Rule.Sat" []
  , Str "$"
  , Call "flat.in" [] -- should be either Rule.Flat or Void; needed to avoid attempting a flatten parse directly inside another flatten, which can happen when there's space before a closing slash
  ]

rule_group :: Rule
rule_group = Ctor "Rule.Group" $ Seq
  [ Str "("
  , opt $ Call "ws" []
  , Call "Rule" [Call "Rule.Flat" []]
  , opt $ Call "ws" []
  , Str ")"
  ]

rule_flat :: Rule
rule_flat = Ctor "Rule.Flat" $ Seq
  [ Str "/"
  , opt $ Call "ws" []
  , Call "Rule" [End] -- TODO for now I'm using End, but this should be Void with an appropriate message
  , opt $ Call "ws" []
  , Str "/"
  ]

rule_sat :: Rule
rule_sat = Ctor "Rule.Sat" $ Seq
  [ Alt [Str "[^", Str "["]
  , Star $ Alt
    [ Call "Char.Range" []
    , Call "Char" []
    , Call "Class.Var" []
    , Flat $ Seq [ Sat brackChar, Many brackChar ]
    , Call "ws" []
    ]
  , Str "]"
  ]
  where
  brackChar = asciiPrint `CS.minus` CS.oneOf "[]\'-:\\ "

rule_char :: Rule
rule_char = Ctor "Rule.Char" $ Seq
  [ Str "\'"
  , Call "char.sq" []
  , Str "\'"
  ]

rule_string :: Rule
rule_string = Ctor "Rule.String" $ Seq
  [ Str "\""
  , Star $ Call "chars.dq" []
  , Str "\""
  ]

rule_call :: Rule
rule_call = Ctor "Rule.Call" $ Seq
  [ Call "Rule.name" []
  , opt $ Seq
    [ Str "<"
    , Call "sep.by"
      [ Seq [ Str ",", opt $ Call "ws" [] ] -- TODO take this (and similar lines) and factor them into their own rule
      , Call "Rule" [Call "Rule.Flat" []]
      ]
    , Str ">"
    ]
  ]

rule_name_ :: Rule
rule_name_ = Flat $
  Call "sep.dot"
    [ Seq
      [ Sat alpha
      , Many alphaNum
      ]
    ]

rule_name_lower_ :: Rule
rule_name_lower_ = Flat $
  Call "sep.dot"
    [ Seq
      [ Sat loAlpha
      , Many loAlphaNum
      ]
    ]

------ Defining Classes ------

classDef :: Rule
classDef = Ctor "Class.Def" $ Seq
  [ Call "Class.Var" []
  , Call "ws" []
  , Str "="
  , Call "ws" []
  , Call "Class.body" []
  ]

class_body_ :: Rule
class_body_ = Seq
  [ Call "Class.term" []
  , Star2 (Call "ws" [])
    $ Seq
      [ Call "Class.operator" []
      , Call "ws" []
      , Call "Class.term" []
      ]
  ]

class_term_ :: Rule
class_term_ = Alt
  [ Expect "class name"        $ Call "Class.Var" []
  , Expect "set"               $ Call "Char.Set" []
  , Expect "range"             $ Call "Char.Range" []
  , Expect "character literal" $ Call "Char" []
  ]

class_operator_ :: Rule
class_operator_ = Alt
  [ Str "|"
  , Str "-"
  ]

class_var :: Rule
class_var = Ctor "Class.Var" $ Seq
  [ Str ":"
  , Call "Class.name" []
  , Str ":"
  ]

class_name_ :: Rule
class_name_ = Flat $
  Call "sep.dot"
    [ Seq
      [ plus (Sat loAlpha)
      , Star $ Seq
        [ Str "-"
        , plus (Sat loAlpha)
        ]
      ]
    ]

------ Literals ------

char :: Rule
char = Ctor "Char" $ Seq
  [ Str "\'"
  , Call "char.sq" []
  , Str "\'"
  ]

char_range :: Rule
char_range = Ctor "Char.Range" $ Seq
  [ Str "\'", Call "char.sq" [], Str "\'"
  , Str ".."
  , Str "\'", Call "char.sq" [], Str "\'"
  ]

char_set :: Rule
char_set = Ctor "Char.Set" $ Seq
  [ Str "\""
  , Star $ Call "chars.dq" []
  , Str "\""
  ]

char_sq_ :: Rule
char_sq_ = Alt
  [ Sat sqCharSet
  , Call "char.Escape" []
  ]
sqCharSet :: CharSet
sqCharSet = CS.contiguous ' ' '~' `CS.minus` CS.oneOf "\'\\"

chars_dq_ :: Rule
chars_dq_ = Alt
  [ Flat $ Sat dqCharSet `Seq2` Many dqCharSet
  , Call "char.Escape" []
  ]
dqCharSet :: CharSet
dqCharSet = CS.contiguous ' ' '~' `CS.minus` CS.oneOf "\"\\"

char_escape :: Rule
char_escape = Ctor "char.Escape" $ Alt
  [ Seq [ Str "\\x", Flat . Seq $ replicate 2 (Sat hexDigit) ]
  , Seq [ Str "\\u", Flat . Seq $ replicate 4 (Sat hexDigit) ]
  , Seq
    [ Str "\\U"
    , Flat $ Alt
      [ Seq $ Str "0"  : replicate 5 (Sat hexDigit)
      , Seq $ Str "10" : replicate 4 (Sat hexDigit)
      ]
    ]
  , Flat $ Seq [ Str "\\", Sat cEscapeChar ]
  , Seq [ Str "\\", Sat asciiPrint ]
  ]

------ Utilities ------

ws_ :: Rule
ws_ = plus1 $ Alt
  [ Call "Space" []
  , Call "Comment" []
  , AsUnit $ Seq
     [ Call "Nl" []
     , Star $ Call "BirdFoot.Improper" []
     , Call "BirdFoot" []
     , Call "ws" []
     ]
  ]

blank_ :: Rule
blank_ = plus1 $ Alt
  [ Seq
    [ Call "BirdFoot" []
    , Star1 $ Call "Space" [] `Alt2` Call "Comment" []
    , Call "Nl" []
    ]
  , Call "BirdFoot.Improper" []
  ]

birdFoot :: Rule
birdFoot = Ctor "BirdFoot" $ Str "> "

birdFoot_improper :: Rule
birdFoot_improper = Ctor "BirdFoot.Improper" $ Str ">\n"

space :: Rule
space = Ctor "Space" $ Flat $ Sat wsChar `Seq2` Many wsChar

nl :: Rule
nl = Ctor "Nl" $ Str "\n"

comment :: Rule
comment = Ctor "Comment" $ Seq
  [ Str "#"
  , Many $ CS.complement (CS.singleton '\n')
  ]

------ General Combinators ------

sep_by_ :: ([String], Rule)
sep_by_ = (["sep", "g"], it)
  where
  it = Seq
    [ Call "g" []
    , Star $ Seq
      [ Call "sep" []
      , Call "g" []
      ]
    ]

sep_dot_ :: ([String], Rule)
sep_dot_ = (["g"], it)
  where it = Call "sep.by" [Str ".", Call "g" []]

num_nat_ :: Rule
num_nat_ = Flat $ Sat digit `Seq2` Many digit

------ Character Sets ------

wsChar :: CharSet
wsChar = CS.oneOf " "

loAlpha :: CharSet
loAlpha = CS.contiguous 'a' 'z'

hiAlpha :: CharSet
hiAlpha = CS.contiguous 'A' 'Z'

alpha :: CharSet
alpha = loAlpha `CS.union` hiAlpha

digit :: CharSet
digit = CS.contiguous '0' '9'

hexDigit :: CharSet
hexDigit =   digit
  `CS.union` CS.contiguous 'a' 'f'
  `CS.union` CS.contiguous 'A' 'F'

alphaNum :: CharSet
alphaNum = alpha `CS.union` digit

loAlphaNum :: CharSet
loAlphaNum = alphaNum `CS.minus` hiAlpha

cEscapeChar :: CharSet
cEscapeChar = CS.oneOf "0abefnrtv"

asciiPrint :: CharSet
asciiPrint = CS.contiguous ' ' '~'

------ Helpers ------

opt :: Rule -> Rule
opt g = g `Alt2` Empty

plus :: Rule -> Rule
plus g = g `Seq2` Star g

plus1 :: Rule -> Rule
plus1 g = g `Seq2` Star1 g
