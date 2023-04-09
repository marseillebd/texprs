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
        , Call "DefClass" []
        ]
      , opt $ Call "ws" []
      , Alt [ Call "Nl" [], Void ]
      ]
    , Call "blank" []
    ]
  -- , Void
  ]

rules :: Map String ([String], Rule)
rules = Map.fromList
  [ ("Doc", ([], doc))
  , ("Doc.line", ([], doc_line_))
  , ("Rule.Def", ([], rule_def))
  , ("Rule.name", ([], rule_name_))
  , ("Rule.Params", ([], rule_params))
  , ("Rule.Params.name", ([], rule_params_name_))
  , ("Rule.Group", (["flat.in"], rule_group))
  , ("Rule", (["flat.in"], rule))
  , ("Rule.Term", (["flat.in"], rule_term))
  , ("Rule.Factor", (["flat.in"], rule_factor))
  , ("Rule.Rep", (["flat.in"], rule_rep))
  , ("Rule.prim", (["flat.in"], rule_prim_))
  , ("Rule.Sat", ([], rule_sat))
  , ("Rule.Char", ([], rule_char))
  , ("Rule.String", ([], rule_string))
  , ("Rule.Call", ([], rule_call))
  , ("Rule.Flat", ([], rule_flat))
  -- TODO below _might_ not be kept
  , ("DefClass", ([], defClass))
  , ("class.body", ([], class_body_))
  , ("class.term", ([], class_term_))
  , ("class.operator", ([], class_operator_))
  , ("ClassVar", ([], classVar))
  , ("className", ([], className_))
  , ("Char", ([], char))
  , ("CharRange", ([], charRange))
  , ("CharSet", ([], charSet))
  , ("sqChar", ([], sqChar_))
  , ("dq.chars", ([], dq_chars_))
  , ("EscapeSeq", ([], escapeSeq))
  , ("sep.by", sep_by_)
  , ("sep.dot", sep_dot_)
  , ("ws", ([], ws_))
  , ("blank", ([], blank_))
  , ("Space", ([], space))
  , ("BirdFoot", ([], birdFoot))
  , ("BirdFoot.Improper", ([], birdFoot_improper))
  , ("Nl", ([], nl))
  , ("Comment", ([], comment))
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
  [ Call "Rule.name" []
  , Call "Rule.Params" []
  , Call "ws" []
  , Str "="
  , Call "ws" []
  , Call "Rule" [Call "Rule.Flat" []]
  ]

rule_params :: Rule
rule_params = Ctor "Rule.Params" $ opt $ Seq
  [ Str "<"
  , Call "sep.by"
    [ Seq [ Str ",", opt $ Call "ws" [] ]
    , Call "Rule.Params.name" []
    ]
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
rule_rep = Ctor "Rule.Rep" $ Seq
  [ Call "Rule.prim" [Call "flat.in" []]
  , Sat $ CS.oneOf "*+?"
  ]

rule_prim_ :: Rule
rule_prim_ = Alt
  [ Call "Rule.Group" [Call "Rule.Flat" []]
  , Call "Rule.Call" []
  , Call "Rule.Char" []
  , Call "Rule.String" []
  , Call "Rule.Sat" []
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
  , Call "Rule" [Void]
  , opt $ Call "ws" []
  , Str "/"
  ]

rule_sat :: Rule
rule_sat = Ctor "Rule.Sat" $ Seq
  [ Alt [Str "[^", Str "["]
  , Star $ Alt
    [ Call "CharRange" []
    , Call "Char" []
    , Seq [ Str ":", Call "className" [], Str ":" ] -- TODO refactor this sequence, and other locations
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
  , sqChar_
  , Str "\'"
  ]

rule_string :: Rule
rule_string = Ctor "Rule.String" $ Seq
  [ Str "\""
  , Star $ Call "dq.chars" []
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

rule_params_name_ :: Rule
rule_params_name_ = Flat $
  Call "sep.dot"
    [ Seq
      [ Sat loAlpha
      , Many loAlphaNum
      ]
    ]

------ Defining Classes ------

defClass :: Rule
defClass = Ctor "DefClass" $ Seq
  [ Str ":", Call "className" [], Str ":"
  , Call "ws" []
  , Str "="
  , Call "ws" []
  , Call "class.body" []
  ]

class_body_ :: Rule
class_body_ = Seq
  [ Call "class.term" []
  , Star2 (Call "ws" [])
    $ Seq
      [ Call "class.operator" []
      , Call "ws" []
      , Call "class.term" []
      ]
  ]

class_term_ :: Rule
class_term_ = Alt
  [ Expect "class name"        $ Call "ClassVar" []
  , Expect "set"               $ Call "CharSet" []
  , Expect "range"             $ Call "CharRange" []
  , Expect "character literal" $ Call "Char" []
  ]

class_operator_ :: Rule
class_operator_ = Alt
  [ Str "|"
  , Str "-"
  ]

classVar :: Rule
classVar = Ctor "ClassVar" $ Seq
  [ Str ":"
  , Call "className" []
  , Str ":"
  ]

className_ :: Rule
className_ = Flat $
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
  , Call "sqChar" []
  , Str "\'"
  ]

charRange :: Rule
charRange = Ctor "CharRange" $ Seq
  [ Str "\'", Call "sqChar" [], Str "\'"
  , Str ".."
  , Str "\'", Call "sqChar" [], Str "\'"
  ]

charSet :: Rule
charSet = Ctor "CharSet" $ Seq
  [ Str "\""
  , Star $ Call "dq.chars" []
  , Str "\""
  ]

sqChar_ :: Rule
sqChar_ = Alt
  [ Sat sqCharSet
  , Call "EscapeSeq" []
  ]
sqCharSet :: CharSet
sqCharSet = CS.contiguous ' ' '~' `CS.minus` CS.oneOf "\'\\"

dq_chars_ :: Rule
dq_chars_ = Alt
  [ Many dqCharSet
  , Call "EscapeSeq" []
  ]
dqCharSet :: CharSet
dqCharSet = CS.contiguous ' ' '~' `CS.minus` CS.oneOf "\"\\"

escapeSeq :: Rule
escapeSeq = Ctor "EscapeSeq" $ Alt
  [ Seq [ Str "\\x", Flat . Seq $ replicate 2 (Sat hexDigit) ]
  , Seq [ Str "\\u", Flat . Seq $ replicate 4 (Sat hexDigit) ]
  , Seq [ Str "\\U", Flat . Seq $ replicate 6 (Sat hexDigit) ]
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
