{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Texpr.Tree where

import Data.CharSet (CharSet)

-- TODO I only have one "layer" of error recovery
  -- if I want to recover token up to whitespace, but outside that also <linestuff> up to newline,
  -- the whitespace recovery could skip the newline I should have stopped at
  -- old algo:
    -- I'll need to track active recoverers in the ParseState, and pick the recovery that will be the shallowest
    -- honestly, could just collect active recoverers from the stack, since I'll also need to know how much stack to shave off once recovery succeeds
    -- however, backtrack should still be tracked as it is (i.e. use the nearest catcher's backtrack)
  -- new algo:
    -- the environment will have to include all recovery handlers that were encountered above this point in the grammar
    -- then, all the handlers will be pulled out and executed, then sorted by ascending length of skip
    -- handlers are of two forms: skipping to the g2 when g2 fails, and skipping to the g2 when g2 fails
    -- for the first type:
      -- if the first handler succeeds, that's it, we're done
      -- if not, we add a character to the skip and reinsert the handler+skip back into our sorted list
      -- if no more characters can be added to the skip, then it's jsut removed as a possibility
    -- for the second: just remove that handler from the list?
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
  | Void
  | Alt2 Rule Rule
  | Empty
  | Seq2 Rule Rule
  | Star2 Rule Rule
  | Ctor String Rule
  | Flat Rule
  | AsUnit Rule -- ^ if the rule fails, fail as soon as the rule started (i.e. like an `Expect`, but no new error message)
  | Expect String Rule
  --- | Fail String -- TODO probably add a Rule arg so that we can say "not expecting a foo when the rule succeeds
  | Call String [Rule] -- lookup a binding in the current environment and match it
  | Capture String Rule Rule -- i.e. capture string as the text matching Rule₁ and use that binding in Rule₂
  | Replay String -- rather than calling, so we don't have to save an environment
  | Recover Rule Rule -- if the first or second fails, skip it, then try second
  deriving (Show,Eq)

pattern Alt :: [Rule] -> Rule
pattern Alt ts <- (fromAlt -> ts@(_:_:_))
  where
  Alt [] = Void
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

pattern Star :: Rule -> Rule
pattern Star g = Star2 Empty g

pattern Star1 :: Rule -> Rule
pattern Star1 g = Star2 g Empty
