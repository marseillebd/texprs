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
      , opt $ Call "Nl" []
      ]
    , Call "blank" []
    ]
  -- , Void
  ]

rules :: Map String ([String], Rule)
rules = Map.fromList
  [ ("Doc", ([], doc))
  , ("Doc.line", ([], doc_line_))
  , ("tbnf", ([], tbnf_))
  , ("tbnf.prim", ([], tbnf_prim_))
  , ("tbnf.Char", ([], tbnf_char))
  , ("tbnf.Call", ([], tbnf_call))
  , ("Rule.Def", ([], rule_def))
  , ("Rule.name", ([], rule_name_))
  , ("Rule.Params", ([], rule_params))
  , ("Rule.Params.name", ([], rule_params_name_))
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
  , ("dqChar", ([], dqChar_))
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

------ Grammars ------

tbnf_ :: Rule
tbnf_ = tbnf_prim_ -- TODO terms, then sequence elements, then repetition base

tbnf_prim_ :: Rule
tbnf_prim_ = Alt
  [ Call "tbnf.Call" []
  , Call "tbnf.Char" []
  ]

tbnf_char :: Rule
tbnf_char = Ctor "tbnf.Char" $ Seq
  [ Str "\'"
  , sqChar_
  , Str "\'"
  ]

tbnf_call :: Rule
tbnf_call = Ctor "tbnf.Call" $ Seq
  [ Call "Rule.name" []
  , opt $ Seq
    [ Str "<"
    , Call "sep.by"
      [ Seq [ Str ",", opt $ Call "ws" [] ] -- TODO take this (and similar lines) and factor them into their own rule
      , Call "tbnf" []
      ]
    , Str ">"
    ]
  ]

------ Defining Rules ------

rule_def :: Rule
rule_def = Ctor "Rule.Def" $ Seq
  [ Call "Rule.name" []
  , Call "Rule.Params" []
  , Call "ws" []
  , Str "="
  , Call "ws" []
  , Call "tbnf.prim" []
  -- TODO
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
  , opt $ Call "ws" []
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
  , Star $ Call "dqChar" []
  , Str "\""
  ]

sqChar_ :: Rule
sqChar_ = Alt
  [ Sat sqCharSet
  , Call "EscapeSeq" []
  ]
sqCharSet :: CharSet
sqCharSet = CS.contiguous ' ' '~' `CS.minus` CS.oneOf "\'\\"

dqChar_ :: Rule
dqChar_ = Alt
  [ Sat dqCharSet
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
sep_by_ = (["sep", "g"], rule)
  where
  rule = Seq
    [ Call "g" []
    , Star $ Seq
      [ Call "sep" []
      , Call "g" []
      ]
    ]

sep_dot_ :: ([String], Rule)
sep_dot_ = (["g"], rule)
  where rule = Call "sep.by" [Str ".", Call "g" []]

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
cEscapeChar = CS.oneOf "abefnrtv"

asciiPrint :: CharSet
asciiPrint = CS.contiguous ' ' '~'

------ Helpers ------

opt :: Rule -> Rule
opt g = g `Alt2` Empty

plus :: Rule -> Rule
plus g = g `Seq2` Star g

plus1 :: Rule -> Rule
plus1 g = g `Seq2` Star1 g
