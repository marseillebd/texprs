{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Texpr.Tree
  ( Rule(..)
  , pattern Alt
  , pattern Seq
  ) where

import Data.CharSet (CharSet)

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
  = Sat CharSet
  | Many CharSet -- ^ just an efficient synonym for a flattened sequence of singleton char sets
  | Str String -- ^ just an efficient synonym for a flattened sequence of singleton char sets
  | End
  | Void String -- ^ includes a message
  | Alt2 Rule Rule
  | Empty
  | Seq2 Rule Rule
  | Star Rule
  | Ctor String Rule
  | Flat Rule
  | AsUnit Rule -- ^ if the rule fails, fail as soon as the rule started (i.e. like an `Expect`, but no new error message)
  | Expect String Rule
  | Call String [Rule] -- lookup a binding in the current environment and match it
  | Capture String Rule Rule -- i.e. capture string as the text matching Rule₁ and use that binding in Rule₂
  | Replay String -- rather than calling, so we don't have to save an environment
  | TexprCtor String -- ^ match a texpr which is a combo with the given tag
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
