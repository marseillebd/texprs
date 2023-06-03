{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Tbnf.Tree
  ( CompiledTbnf(..)
  , Rule(..)
  , pattern Alt
  , pattern Seq
  -- * Special Names
  , RuleName
  , ruleNameFromString
  , ruleNameToString
  , ParamName
  , paramNameFromString
  , paramNameToString
  , ruleNameFromParamName
  , paramNameFromRuleName
  ) where

import Control.Monad (forM_)
import Data.CharSet (CharSet)
import Data.Map (Map)
import Data.String (IsString(..))
import Data.Texpr (CtorName,ctorNameFromString)
import Data.Text (Text)

import qualified Data.Text as T

-- | An internal representation of a compiled Tbnf grammar, ready to be used as
-- a reader.
--
-- See 'Text.Tbnf.Read.Generic.runReader'.
data CompiledTbnf = Tbnf
  { rules :: Map RuleName ([ParamName], Rule)
  , startRule :: Rule
  }
  deriving (Eq)

-- TODO factor out common prefixes
  -- that is, when there's an alternation with two branches that share a common grammar prefix, I want to do the parse once and re-use the results
    -- so, `Factor Id Rule Rule`, then a `Substitute Id`, usable like
    -- transform Alt [ Ctor "StringLit" $ Seq [Str "\"", strChars, Str "\""]
    --               , Ctor "StringTempl" $ Seq [Str "\"", strChars, Str "`", Star templateContent, Str "\"", strChars, Str "\""]]
    -- into Factor "x" (Seq [Str "\"", strChars])
    --                 (Alt [ Ctor "StringLit" $ Seq [Sub "x",Str "\""]
    --                      , Ctor "StringTempl" $ Seq [Sub "x",Str "`",Star templateContent,Str "\"", strChars,Str "\""]
    --                      ])
    -- this will need the state to hold a map from variables to parse states
    -- oh, it'd be even better if I could do this through a rule call
data Rule
  = Any
  | Sat CharSet
  | Many CharSet -- ^ just an efficient synonym for a flattened sequence of singleton char sets
  | Str Text -- ^ just an efficient synonym for a flattened sequence of singleton char sets
  | End
  | Void Text -- ^ includes a message describing expected input
  | Alt2 Rule Rule
  | Empty
  | Seq2 Rule Rule
  | Star Rule
  | Lookahead Rule
  | NegLookahead Text Rule -- ^ includes a message describing expected input
  | Ctor CtorName Rule
  | Flat Rule
  | Expect Rule Text
  | Call RuleName [Rule] -- lookup a binding in the current environment and match it
  | Capture ParamName Rule Rule -- i.e. capture string as the text matching Rule₁ and use that binding in Rule₂
  | Replay ParamName -- rather than calling, so we don't have to save an environment
  | TexprCombo CtorName -- ^ match a texpr which is a combo with the given tag
  deriving (Show,Eq)

pattern Alt :: [Rule] -> Rule
pattern Alt ts <- (fromAlt -> ts@(_:_:_))
  where
  Alt [] = Void "anything"
  Alt (t:ts) = foldl Alt2 t ts
fromAlt :: Rule -> [Rule]
fromAlt (Alt2 g1 g2) = fromAlt g1 <> fromAlt g2
fromAlt g = [g]

pattern Seq :: [Rule] -> Rule
pattern Seq ts <- (fromSeq -> ts@(_:_:_))
  where
  Seq [] = Empty
  Seq (t:ts) = foldl Seq2 t ts
fromSeq :: Rule -> [Rule]
fromSeq (Seq2 g1 g2) = fromSeq g1 <> fromSeq g2
fromSeq g = [g]

------------------ Special Names ------------------

-- | A newtype restricting the format of parameter name strings.
-- Specifically, parameter names should be simple identifiers allowing internal dashes.
-- Formally, they must match the regex:
--
-- >  [a-zA-Z_][a-zA-Z0-9_]*(-[a-zA-Z_][a-zA-Z0-9_]*)*
--
-- Or, using tbnf:
--
-- >  start = ident-simple ('-' ident-simple)*
-- >  ident-simple = [a-zA-Z_][a-zA-Z0-9_]*
newtype ParamName = ParamName { unParamName :: Text }
  deriving (Eq, Ord)
instance Show ParamName where
  show = show . paramNameToString
instance IsString ParamName where
  fromString str = case paramNameFromString str of
    Nothing -> error $ "invalid texpr PEG parameter name: " ++ show str
    Just cname -> cname

-- | Convert a string to a 'ParamName' if the input is valid.
paramNameFromString :: String -> Maybe ParamName
paramNameFromString str = do
  ParamName . T.pack <$> identSimpleFromString str

-- | Convert a 'ParamName' back into a plain string.
paramNameToString :: ParamName -> String
paramNameToString = T.unpack . unParamName

-- | A newtype restricting the format of rule name strings.
-- Specifically, rule names should be dot-separated identifiers,
-- where identifiers may contain internal dashes.
-- Formally, they must match the regex:
--
-- >  [a-zA-Z_][a-zA-Z0-9_]*(-[a-zA-Z_][a-zA-Z0-9_]*)*(\.[a-zA-Z_][a-zA-Z0-9_]*(-[a-zA-Z_][a-zA-Z0-9_]*)*)*
--
-- Or, using tbnf:
--
-- >  start = ident ('.' ident)*
-- >  ident = ident-simple ('-' ident-simple)*
-- >  ident-simple = [a-zA-Z_][a-zA-Z0-9_]*
newtype RuleName = RuleName { unRuleName :: Text }
  deriving (Eq, Ord)
instance Show RuleName where
  show = show . ruleNameToString
instance IsString RuleName where
  fromString str = case ruleNameFromString str of
    Nothing -> error $ "invalid texpr PEG rule name: " ++ show str
    Just cname -> cname

-- | Convert a string to a 'RuleName' if the input is valid.
ruleNameFromString :: String -> Maybe RuleName
ruleNameFromString str = do
  parts <- splitDots str
  forM_ parts $ identSimpleFromString
  pure $ RuleName $ T.pack str

-- | Convert a 'RuleName' back into a plain string.
ruleNameToString :: RuleName -> String
ruleNameToString = T.unpack . unRuleName

identSimpleFromString :: String -> Maybe String
identSimpleFromString str = ctorNameFromString str >> pure str

splitDots :: String -> Maybe [String]
splitDots [] = Nothing
splitDots ('.':_) = Nothing
splitDots str0 = loop str0
  where
  loop str = case break (=='.') str of
    ([], _:_) -> Nothing
    (_, '.':[]) -> Nothing
    ([], []) -> Just []
    (part, []) -> Just [part]
    (part, '.':str') -> (part :) <$> splitDots str'
    (_, _:_) -> Nothing

ruleNameFromParamName :: ParamName -> RuleName
ruleNameFromParamName = RuleName . unParamName

paramNameFromRuleName :: RuleName -> Maybe ParamName
paramNameFromRuleName (RuleName str) =
  if "." `T.isInfixOf` str
    then Nothing
    else Just (ParamName str)
