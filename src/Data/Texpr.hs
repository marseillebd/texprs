{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the 'Texpr' data type which is core to this entire package..
module Data.Texpr
  ( Texpr(..)
  , Texprs
  , start
  , end
  , range
  -- * To Unstructured
  , flatten
  , unparse
  -- * Dedicated String Types
  , CtorName
  , ctorNameFromString
  , ctorNameToString
  ) where

import Control.Monad (forM_,when)
import Data.Char (isAscii,isAlphaNum,isDigit)
import Data.List (isPrefixOf,intercalate)
import Data.String (IsString(..))
import Data.Text (Text)
import Text.Location (Position,FwdRange,fwd)

import qualified Data.Text as T

-- | Just a synonym for multiple 'Texpr's so I'm not always writing the brackets.
-- Maybe it's not worth it? But streams of t-exprs come up extremely often.
type Texprs = [Texpr]

-- | T-expressions (or t-expr/texpr for short) are a variation on s-expressions
--   where every combination recieves an explicit constructor.
--
-- In plain language, a t-expr is a rose tree of strings,
-- where each non-leaf node is tagged with a "constructor",
-- which is just a kebab-case string.
--
-- T-exprs are specifically intended for use in representing freshly-parsed
-- abstract syntax trees of any grammar.
-- In particular, every node of a t-expr is tagged with a 'Position'
-- from the source text to aid in error reporting and syntax highlighting.
-- Once all textual operations are completed on the source, I expect the t-expr
-- to be consumed and transformed into an appropriate abstract data type that
-- represents the specific grammar that was parsed against.
-- Until then, t-exprs serve as a useful middle point
-- between raw text and fully-abstract syntax.
--
-- Why the name t-expr? What does the @t@ stand for?
-- Well, it could stand for many things:
-- 
-- - for "tagged-expressions", because of the explicit tagging of combinations,
-- - or perhaps "text-expressions", as they hold only text
-- - or perhaps "tree-expressions", as they are meant for nothing more than syntax trees,
--   (as opposed to binding "trees" which are really a form of graph),
-- - or maybe even "token-expression", as it contains all the (possibly nested) tokens of a source file,
-- - or cheekily, because @s++ === t@.
data Texpr
  = Atom {-# UNPACK #-} !FwdRange String -- TODO use Text instead of String
  | Combo {-# UNPACK #-} !FwdRange CtorName Texprs

-- | Get the 'Position' of the start of the given 'Texpr'.
start :: Texpr -> Position
start (Atom r _) = r.anchor
start (Combo r _ _) = r.anchor

-- | Get the 'Position' of the end of the given 'Texpr'.
end :: Texpr -> Position
end (Atom r _) = r.position
end (Combo r _ _) = r.position

-- | Get the start and end 'Position's of the given 'Texpr'.
range :: Texpr -> FwdRange
range t = fwd (start t) (end t)

-- | Concatenate all nodes of the given forest of 'Texpr's in inorder traversal.
-- Produces an atom whose range is computed based on
-- the start of the first 'Texpr' and end of the last.
-- If the forest is empty, then a location cannot be created
-- so the result is 'Nothing'.
flatten :: [Texpr] -> Maybe Texpr -- TODO check uses of this to ensure I'm not dropping useful things
flatten [] = Nothing
flatten ts0 = Just . Atom p . goList $ ts0
  where
  p = fwd (start $ head ts0) (end $ last ts0)
  goList ts = concat $ goTree <$> ts
  goTree (Atom _ str) = str
  goTree (Combo _ _ ts) = goList ts

-- | Concatenate all the text of the given 'Texpr' in inorder traversal.
--
-- If the given 'Texpr' was generated from parsing source text without dropping
-- characters, then 'unparse' will return the original source text.
unparse :: Texpr -> String
unparse (Atom _ str) = str
unparse (Combo _ _ ts) = concat (unparse <$> ts)

------------------ Rendering ------------------

instance Show Texpr where
  show (Atom _ str) = show str
  show (Combo _ name children) = concat
    [ "("
    , ctorNameToString name
    , concatMap (' ':) $ show <$> children
    , ")"
    ]

------------------ Constructor Names ------------------

-- | A newtype restricting the format of constructor name strings.
-- Specifically, constructor names should be kebab-case identifiers.
-- Formally, they must match the regex:
--
-- >  [a-zA-Z_][a-zA-Z0-9_]*(-[a-zA-Z_][a-zA-Z0-9_]*)*
newtype CtorName = CtorName { unCtorName :: Text }
  deriving (Eq, Ord)
instance Show CtorName where
  show = show . ctorNameToString
instance IsString CtorName where
  fromString str = case ctorNameFromString str of
    Nothing -> error $ "invalid texpr constructor name: " ++ show str
    Just cname -> cname

-- | Convert a string to a 'CtorName' if the input is valid.
ctorNameFromString :: String -> Maybe CtorName
ctorNameFromString str = do
  when ("-" `isPrefixOf` str) Nothing
  parts <- splitDashes str
  forM_ parts $ \part -> do
    when (isDigit $ head part) Nothing
    when (not $ all isIdChar part) Nothing
  pure $ CtorName $ T.pack $ intercalate "-" parts
  where
  isIdChar c = isAscii c && isAlphaNum c || c == '_'

splitDashes :: String -> Maybe [String]
splitDashes str = case break (=='-') str of
  ([], _:_) -> Nothing
  (_, '-':[]) -> Nothing
  ([], []) -> Just []
  (part, []) -> Just [part]
  (part, '-':str') -> (part :) <$> splitDashes str'
  (_, _:_) -> Nothing

-- | Convert a 'CtorName' back into a plain string.
ctorNameToString :: CtorName -> String
ctorNameToString = T.unpack . unCtorName
