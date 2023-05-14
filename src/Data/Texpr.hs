{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Data.Texpr
  ( Texpr(..)
  , Texprs
  , start
  , end
  , range
  -- * To Unstructured
  , flatten
  , unparse
  ) where

import Text.Location (Position,FwdRange,fwd)

-- for "term-expressions", which are s-expressions with string atoms, where combinations are always tagged with an explicit constructor (also a string)
-- or perhaps "text-expressions", as they hold only text
-- or perhaps "tree-expressions", as they hold syntax trees (as opposed to binding "trees" which are really a form of graph)
-- or maybe even "token-expression", as it contains all the (possibly nested) tokens of a source file
-- or cheekily, because s++ would be t
type Texprs = [Texpr]
data Texpr
  = Atom {-# UNPACK #-} !FwdRange String
  | Combo FwdRange String Texprs

start :: Texpr -> Position
start (Atom r _) = r.anchor
start (Combo r _ _) = r.anchor
end :: Texpr -> Position
end (Atom r _) = r.position
end (Combo r _ _) = r.position

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

unparse :: Texpr -> String
unparse (Atom _ str) = str
unparse (Combo _ _ ts) = concat (unparse <$> ts)


------------------ Rendering ------------------

instance Show Texpr where
  show (Atom _ str) = show str
  show (Combo _ name children) = concat ["(", name, concatMap (' ':) $ show <$> children, ")"]
