module Data.Texpr where

import Data.CharSet (CharSet)

import qualified Data.CharSet as CharSet

-- for "term-expressions", which are s-expressions with string atoms, where combinations are always tagged with an explicit constructor (also a string)
-- or perhaps "text-expressions", as they hold only text
-- or perhaps "tree-expressions", as they hold syntax trees (as opposed to binding "trees" which are really a form of graph)
-- or maybe even "token-expression", as it contains all the (possibly nested) tokens of a source file
-- or cheekily, because s++ would be t
type Texprs = [Texpr]
data Texpr
  = Atom {-# UNPACK #-} !Pos String -- should never be empty
  | Combo String Int Texprs
  | Error (Pos, String) Error (Pos, String) -- text before error, error that was caught, and skipped input

instance Show Texpr where
  show (Atom _ str) = show str
  show (Combo name _ children) = concat ["(", name, concatMap (' ':) $ show <$> children, ")"]
  show (Error (_, preErr) err (_, skip)) = concat
    [ "!{"
    , if null preErr then "" else show preErr <> " "
    , "(" <> showErr err <> ")"
    , if null skip then "" else " " <> show skip
    , "}"
    ]
showErr :: Error -> String
showErr err = case err of
  ExpectingNothing -> "ExpectingEnd"
  ExpectingCharIn cs -> "ExpectingChar " <> show (CharSet.render cs)
  ExpectingString txt -> "ExpectingString " <> show txt
  ExpectingName name (_, txt') err' -> concat
    [ "Expecting "
    , show name
    , " ("
    , if null txt' then "" else show txt' <> " "
    , showErr err'
    , ")"
    ]
  CustomError msg -> "Error " <> show msg

data Pos = Pos
  {-# UNPACK #-} !Int -- ^ start
  {-# UNPACK #-} !Int -- ^ end
  deriving (Show)

data Error
  = ExpectingNothing
  | ExpectingCharIn CharSet
  | ExpectingString String
  | ExpectingName String (Int, String) Error -- includes internal error
  | CustomError String
  deriving (Show)

start :: Texpr -> Int
start (Atom (Pos pos _) _) = pos
start (Combo _ pos []) = pos
start (Combo _ _ ts) = start $ head ts
start (Error (Pos pos _, _) _ _) = pos
end :: Texpr -> Int
end (Atom (Pos _ pos) _) = pos
end (Combo _ pos []) = pos
end (Combo _ _ ts) = end $ last ts
end (Error _ _ (Pos _ pos, _)) = pos

range :: Texpr -> Pos
range t = Pos (start t) (end t)

flatten :: [Texpr] -> Maybe Texpr
flatten [] = Nothing
flatten ts0 = Just . Atom pos0 . goList $ ts0
  where
  pos0 = Pos (start $ head ts0) (end $ last ts0)
  goList ts = concat $ goTree <$> ts
  goTree (Atom _ txt) = txt
  goTree (Combo _ _ ts) = goList ts
  goTree (Error _ _ _) = []

unparse :: Texpr -> String
unparse (Atom _ txt) = txt
unparse (Combo _ _ ts) = concat (unparse <$> ts)
unparse (Error (_, preErr) _ (_, skip)) = preErr <> skip
