{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Texpr.Monad
  ( runPeg
  , ErrorReport(..)
  ) where

import Prelude hiding (fail,sequence)

import Data.CharSet (CharSet)
import Data.List (stripPrefix)
import Data.Map (Map)
import Data.Bifunctor (first,second)
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Data.Texpr (Texprs,Texpr(..),Reason(..),noReason,flatten,unparse)
import Data.These (These(..))
import Text.Location (Input(..),Position(..),FwdRange,fwd)
import Text.Texpr.Tree (Rule(..))

import qualified Data.CharSet as CS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Location as Loc

runPeg :: Map String ([String], Rule) -> Rule -> Input -> Either ErrorReport (Texprs, Input)
runPeg ruleSet startRule inp0 = case go of
  This ok -> Right ok
  That err -> Left err
  These ok _ -> Right ok
  where
  go = unParse (parse startRule) env0 inp0
  env0 = Env { global = ruleSet, local = Map.empty, captures = Map.empty }

------ Parsing ------

parse :: Rule -> Parse Texprs
parse = \case
  Sat cs -> satisfy cs
  Many cs -> many cs
  Str str -> string str
  End -> end
  Void msg -> void msg
  Alt2 g1 g2 -> alternate g1 g2
  Empty -> pure []
  Seq2 g1 g2 -> sequence g1 g2
  Star g -> star g
  Ctor name g -> ctor name g
  Flat g -> flat g
  AsUnit g -> asUnit g
  Expect desc g -> expect desc g
  -- Fail msg -> fail msg
  Call f gs -> call f gs
  Capture x g1 g2 -> capture x g1 g2
  Replay x -> replay x

satisfy :: CharSet -> Parse Texprs
satisfy cs = Parse $ \env inp -> case inp.txt of
  c:txt' | c `CS.elem` cs -> This ([t], Input loc' txt')
    where
    t = Atom (fwd inp.loc loc') (c:"")
    loc' = Loc.advance inp.loc [c]
  _ -> unParse (throw explain) env inp
    where explain = (noReason inp.loc){expectingChars = cs} -- WARNING I'm assuming cs is a non-empty set

many :: CharSet -> Parse Texprs
many cs = Parse $ \_ inp -> case span (`CS.elem` cs) inp.txt of
  (ok, txt') -> This ([t], Input loc' txt')
    where
    t = Atom (fwd inp.loc loc') ok
    loc' = Loc.advance inp.loc ok

string :: String -> Parse Texprs
string str = Parse $ \env inp -> case stripPrefix str inp.txt of
  Just txt' -> This ([t], Input loc' txt')
    where
    t = Atom (fwd inp.loc loc') str
    loc' = Loc.advance inp.loc str
  Nothing -> unParse (throw explain) env inp
    where explain = (noReason inp.loc){expectingKeywords = Set.singleton str}

end :: Parse Texprs
end = Parse $ \env inp -> case inp.txt of
  "" -> This ([], inp)
  _ -> unParse (throw explain) env inp
    where explain = (noReason inp.loc){expectingEndOfInput = True}

void :: String -> Parse Texprs
void msg = Parse $ \env inp ->
  let explain = (noReason inp.loc){unexpected = Set.singleton msg}
   in unParse (throw explain) env inp

alternate :: Rule -> Rule -> Parse Texprs
alternate g1 g2 = do
  catch (parse g1) >>= \case
    Right ok -> pure ok
    Left err1 -> parse g2 `mapErr` (err1 <>)

sequence :: Rule -> Rule -> Parse Texprs
sequence g1 g2 = do
  ts1 <- parse g1
  ts2 <- parse g2 `mapErr` \err -> err{prior = ts1 <> err.prior}
  pure $ ts1 <> ts2

star :: Rule -> Parse Texprs
star g = do
  inp0 <- getInput
  catch (parse g) >>= \case
    Left _ -> pure []
    Right ts -> do
      inp' <- getInput
      if inp'.loc == inp0.loc
        then pure [] -- to prevent infinite loops when the repeated grammar accepts empty
        else (ts <>) <$> mapErr (star g) (\err -> err{prior = ts <> err.prior})

ctor :: String -> Rule -> Parse Texprs
ctor name g = do
  (r, ts) <- withRange $ parse g
  pure [Combo r name ts]

flat :: Rule -> Parse Texprs
flat g = do
  ts <- parse g
  pure $ maybeToList (flatten ts)

asUnit :: Rule -> Parse Texprs
asUnit g = parse g `mapErr` \err -> err{prior = []}

expect :: String -> Rule -> Parse Texprs
expect desc g = do
  inp0 <- getInput
  parse g `mapErr` \err -> Err
    { prior = []
    , remaining = inp0
    , reason = (noReason inp0.loc)
                    {expectingByName = Map.singleton desc err.reason}
    }

-- fail :: String -> Parse Texprs
-- fail msg = Parse $ \_ inp -> Left ([], inp, CustomError msg)

call :: String -> [Rule] -> Parse Texprs
call f args = lookupLocal f >>= \case
  Just g -> withCall Map.empty $ parse g
  Nothing -> lookupGlobal f >>= \case
    Just (params, g)
      | length params == length args -> do
        args' <- subst `mapM` args
        let locals' = Map.fromList (zip params args')
        withCall locals' $ parse g
      | otherwise -> errorWithoutStackTrace $ "internal Texpr-Peg error: wrong number of arguments " ++ show f ++ " " ++ show (length args)
    Nothing -> errorWithoutStackTrace $ "internal Texpr-Peg error: unbound grammar " ++ show f

subst :: Rule -> Parse Rule
subst = \case
  Sat cs -> pure $ Sat cs
  Many cs -> pure $ Many cs
  Str txt -> pure $ Str txt
  End -> pure End
  Void msg -> pure $ Void msg
  Alt2 g1 g2 -> Alt2 <$> subst g1 <*> subst g2
  Empty -> pure Empty
  Seq2 g1 g2 -> Seq2 <$> subst g1 <*> subst g2
  Star g -> Star <$> subst g
  Ctor name g -> Ctor name <$> subst g
  Flat g -> Flat <$> subst g
  AsUnit g -> AsUnit <$> subst g
  Expect desc g -> Expect desc <$> subst g
  -- Fail msg -> pure $ Fail msg
  Call f [] -> lookupLocal f >>= \case
    Just g -> pure g
    Nothing -> pure $ Call f []
  Call f gs -> Call f <$> mapM subst gs
  Capture x g1 g2 -> Capture x <$> subst g1 <*> subst g2
  Replay x -> lookupCapture x >>= \case
    Just txt -> pure $ Str txt
    Nothing -> errorWithoutStackTrace  $ "internal Texpr-Peg error: unbound capture " ++ show x

capture :: String -> Rule -> Rule -> Parse Texprs
capture x g1 g2 = do
  ts1 <- parse g1
  let action' = withCapture x (unparse `concatMap` ts1) (parse g2)
  ts2 <- action' `mapErr` \err -> err{prior = ts1 <> err.prior}
  pure $ ts1 <> ts2

replay :: String -> Parse Texprs
replay x = lookupCapture x >>= \case
  Just str -> string str
  Nothing -> errorWithoutStackTrace  $ "internal Texpr-Peg error: unbound capture " ++ show x

------ The Monad ------

newtype Parse a = Parse { unParse :: Env -> Input -> These (a, Input) ErrorReport }

data Env = Env
  { global :: Map String ([String], Rule)
  , local :: Map String Rule
  , captures :: Map String String
  }
  deriving (Show)

data ErrorReport = Err
  { prior :: Texprs
  , reason :: Reason
  , remaining :: Input
  }
  deriving (Show)

instance Semigroup ErrorReport where
  a <> b = case a.remaining.loc `compare` b.remaining.loc of
    GT -> a
    EQ -> a{reason = a.reason <> b.reason}
    LT -> b

instance Functor Parse where
  fmap f (Parse action) = Parse $ \env inp -> first (first f) $ action env inp

instance Applicative Parse where
  pure x = Parse $ \_ inp -> This (x, inp)
  Parse action <*> Parse action' = Parse $ \env inp -> case action env inp of
    This (f, inp') -> first (first f) $ action' env inp'
    That err -> That err
    These (f, inp') err -> case action' env inp' of
      This ok -> These (first f ok) err
      That err' -> That (err <> err')
      These ok err' -> These (first f ok) (err <> err')

instance Monad Parse where
  Parse action >>= k = Parse $ \env inp -> case action env inp of
    This (x, inp') -> unParse (k x) env inp'
    That err -> That err
    These (x, inp') err -> case unParse (k x) env inp' of
      This ok -> These ok err
      That err' -> That (err <> err')
      These ok err' -> These ok (err <> err')

throw :: Reason -> Parse a
throw reason = Parse $ \_ remaining -> That $
  Err { prior = [], reason, remaining }

rethrow :: ErrorReport -> Parse a
rethrow err = Parse $ \_ _ -> That err

catch :: Parse a -> Parse (Either ErrorReport a)
catch action = Parse $ \env inp -> case unParse action env inp of
  This (ok, inp') -> This (Right ok, inp')
  That err -> These (Left err, inp) err
  These (ok, inp') err -> These (Right ok, inp') err

mapErr :: Parse a -> (ErrorReport -> ErrorReport) -> Parse a
mapErr action f = Parse $ \env inp -> second f $ unParse action env inp

getInput :: Parse Input
getInput = Parse $ \_ inp -> This (inp, inp)

getPosition :: Parse Position
getPosition = Parse $ \_ inp -> This (inp.loc, inp)

withRange :: Parse a -> Parse (FwdRange, a)
withRange action = do
  p0 <- getPosition
  x <- action
  p' <- getPosition
  pure (fwd p0 p', x)

withCall :: Map String Rule -> Parse a -> Parse a
withCall local action = Parse $ \env inp -> unParse action env{local,captures=Map.empty} inp

withCapture :: String -> String -> Parse a -> Parse a
withCapture x v action = Parse $ \env inp ->
  let env' = env{captures = Map.insert x v env.captures}
   in unParse action env' inp

lookupGlobal :: String -> Parse (Maybe ([String], Rule))
lookupGlobal x = Parse $ \env inp -> This (Map.lookup x env.global, inp)

lookupLocal :: String -> Parse (Maybe Rule)
lookupLocal x = Parse $ \env inp -> This (Map.lookup x env.local, inp)

lookupCapture :: String -> Parse (Maybe String)
lookupCapture x = Parse $ \env inp -> This (Map.lookup x env.captures, inp)

------ Error Recovery ------

data Next = Next
  { expectChars :: CharSet
  , expectStrs :: Set String
  , acceptEmpty :: Bool
  }

instance Semigroup Next where
  a <> b = Next
    { expectChars = a.expectChars <> b.expectChars
    , expectStrs = a.expectStrs <> b.expectStrs
    , acceptEmpty = a.acceptEmpty || b.acceptEmpty
    }
instance Monoid Next where
  mempty = Next CS.empty Set.empty False
