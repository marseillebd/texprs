{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Data.Texpr where

import Data.CharSet (CharSet)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Set (Set)
import Text.Location (Source(..),Position,FwdRange,fwd)

import qualified Data.CharSet as CS
import qualified Data.Map as Map
import qualified Data.Set as Set

-- for "term-expressions", which are s-expressions with string atoms, where combinations are always tagged with an explicit constructor (also a string)
-- or perhaps "text-expressions", as they hold only text
-- or perhaps "tree-expressions", as they hold syntax trees (as opposed to binding "trees" which are really a form of graph)
-- or maybe even "token-expression", as it contains all the (possibly nested) tokens of a source file
-- or cheekily, because s++ would be t
type Texprs = [Texpr]
data Texpr
  = Atom {-# UNPACK #-} !FwdRange String
  | Combo FwdRange String Texprs
  | Error Source Reason Source -- text before error, error that was caught, and skipped input

start :: Texpr -> Position
start (Atom r _) = r.anchor
start (Combo r _ _) = r.anchor
start (Error preErr _ _) = preErr.loc.anchor
end :: Texpr -> Position
end (Atom r _) = r.position
end (Combo r _ _) = r.position
end (Error _ _ skip) = skip.loc.position

range :: Texpr -> FwdRange
range t = fwd (start t) (end t)

flatten :: [Texpr] -> Maybe Texpr -- TODO check uses of this to ensure I'm not dropping useful things
flatten [] = Nothing
flatten ts0 = Just . Atom p . goList $ ts0
  where
  p = fwd (start $ head ts0) (end $ last ts0)
  goList ts = concat $ goTree <$> ts
  goTree (Atom _ str) = str
  goTree (Combo _ _ ts) = goList ts
  goTree (Error _ _ _) = []

unparse :: Texpr -> String
unparse (Atom _ str) = str
unparse (Combo _ _ ts) = concat (unparse <$> ts)
unparse (Error preErr _ skip) = preErr.txt <> skip.txt

------------------ Errors ------------------

data Reason = Reason
  { expectAt :: Position
  , expectingEndOfInput :: Bool
  , expectingChars :: CharSet
  , expectingKeywords :: Set String
  , expectingByName :: Map String Reason
  }

noReason :: Position -> Reason
noReason expectAt = Reason
  { expectAt
  , expectingEndOfInput = False
  , expectingChars = CS.empty
  , expectingKeywords = Set.empty
  , expectingByName = Map.empty
  }

instance Semigroup Reason where
  a <> b = case a.expectAt `compare` b.expectAt of
    GT -> a
    EQ -> Reason
      { expectAt = a.expectAt
      , expectingEndOfInput = a.expectingEndOfInput || b.expectingEndOfInput
      , expectingChars = a.expectingChars <> b.expectingChars
      , expectingKeywords = a.expectingKeywords <> b.expectingKeywords
      , expectingByName = Map.unionWith (<>) a.expectingByName b.expectingByName
      }
    LT -> b

------------------ Rendering ------------------

instance Show Texpr where
  show (Atom _ str) = show str
  show (Combo _ name children) = concat ["(", name, concatMap (' ':) $ show <$> children, ")"]
  show (Error preErr err skip) = concat
    [ "!{"
    , if null preErr.txt then "" else show preErr.txt <> " "
    , "(" <> show err <> ")" -- TODO use a more s-expr-oriented renderer
    , if null skip.txt then "" else " " <> show skip.txt
    , "}"
    ]

instance Show Reason where
  show r = concat
    [ "(noReason "
    , show r.expectAt
    , "){"
    , intercalate "," $ concat
      [ if r.expectingEndOfInput then ["expectingEndOfInput=True"] else []
      , if CS.null r.expectingChars then [] else ["expectingChars=" <> show r.expectingChars]
      , if Set.null r.expectingKeywords then [] else ["expectingKeywords=" <> show r.expectingKeywords]
      , if Map.null r.expectingByName then [] else ["expectingByName=" <> show r.expectingByName]
      ]
    , "}"
    ]
