{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module provides tools for defining readers over various types of input
-- stream.
-- The readers use 'Text.Tbnf.Tbnf' grammars to match against input and
-- produce 'Texprs'.
--
-- The most important interface here is the 'Stream' class.
-- If you are implementing 'Stream' for yourself, you may find it useful to
--   look in "Text.Tbnf.Read.String" or "Text.Tbnf.Read.Texpr" for examples.
--
-- We call these readers rather than parsers in the tradition of Lisp.
-- In Lisp, a reader takes an input string to an s-expression,
--   and from there the s-expression is then parsed (by special forms and macros)
--   to recognize the abstract syntax of Lisp.
-- Because t-exprs take on a role similar to s-exprs, we will use this same terminology.
-- (Rant: because I'm so tired of getting a zillion new names for the same thing,
-- and while I'm at it, also a zillion things that all go by the same name.
-- Will programmers ever learn their own history?)
module Text.Tbnf.Read.Generic
  ( runReader
  , ReaderError(..)
  , Reason(..)
  , noReason
  , Stream(..)
  ) where

import Prelude hiding (fail,sequence)

import Text.Tbnf.Read.Monad

import Data.CharSet (CharSet)
import Data.Maybe (maybeToList)
import Data.Texpr (Texprs,Texpr(..),flatten,unparse,CtorName)
import Data.Text (Text)
import Data.These (These(..))
import Text.Location (fwd)
import Text.Tbnf.Tree (CompiledTbnf(..),Rule(..),RuleName,ParamName)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Create a stream of 'Texpr's from an input stream that matches the given
-- grammar, or report an error.
runReader :: (Stream s)
  => CompiledTbnf
  -> s -- ^ input
  -> Either (ReaderError s) (Texprs, s) -- ^ result with remaining input
runReader tbnf inp0 = case go of
  This ok -> Right ok
  That err -> Left err
  These ok _ -> Right ok
  where
  go = unParse (parse tbnf.startRule) env0 inp0
  env0 = Env { global = tbnf.rules, local = Map.empty, captures = Map.empty }

------ Parsing ------

parse :: (Stream s) => Rule -> Parse s Texprs
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
  Expect g msg -> expect g msg
  Call f gs -> call f gs
  Capture x g1 g2 -> capture x g1 g2
  Replay x -> replay x
  TexprCombo name -> combo name

satisfy :: (Stream s) => CharSet -> Parse s Texprs
satisfy cs = Parse $ \env inp -> case takeChar cs inp of
  Just (c, inp') ->
    let loc = fwd (location inp) (location inp')
     in This ([Atom loc (T.singleton c)], inp')
  Nothing -> unParse (throw explain) env inp
    where explain = (noReason $ location inp){expectingChars = cs} -- WARNING I'm assuming cs is a non-empty set

many :: (Stream s) => CharSet -> Parse s Texprs
many cs = Parse $ \_ inp ->
  let (txt, inp') = takeChars cs inp
      loc = fwd (location inp) (location inp')
   in This ([Atom loc txt], inp')

string :: (Stream s) => Text -> Parse s Texprs
string str = Parse $ \env inp -> case stripStringPrefix str inp of
  Just inp' ->
    let loc = fwd (location inp) (location inp')
     in This ([Atom loc str], inp')
  Nothing -> unParse (throw explain) env inp
    where explain = (noReason $ location inp){expectingKeywords = Set.singleton str}

end :: (Stream s) => Parse s Texprs
end = Parse $ \env inp -> case isAtEnd inp of
  True -> This ([], inp)
  False -> unParse (throw explain) env inp
    where explain = (noReason $ location inp){expectingEndOfInput = True}

void :: (Stream s) => Text -> Parse s Texprs
void msg = Parse $ \env inp ->
  let explain = (noReason $ location inp){unexpected = Set.singleton msg}
   in unParse (throw explain) env inp

alternate :: (Stream s) => Rule -> Rule -> Parse s Texprs
alternate g1 g2 = do
  catch (parse g1) >>= \case
    Right ok -> pure ok
    Left err1 -> parse g2 `mapErr` (err1 <>)

sequence :: (Stream s) => Rule -> Rule -> Parse s Texprs
sequence g1 g2 = do
  ts1 <- parse g1
  ts2 <- parse g2 `mapErr` \err -> err{prior = ts1 <> err.prior}
  pure $ ts1 <> ts2

star :: (Stream s) => Rule -> Parse s Texprs
star g = do
  inp0 <- getInput
  catch (parse g) >>= \case
    Left _ -> pure []
    Right ts -> do
      inp' <- getInput
      if location inp' == location inp0
        then pure [] -- to prevent infinite loops when the repeated grammar accepts empty
        else (ts <>) <$> mapErr (star g) (\err -> err{prior = ts <> err.prior})

ctor :: (Stream s) => CtorName -> Rule -> Parse s Texprs
ctor name g = do
  (r, ts) <- withRange $ parse g
  pure [Combo r name ts]

flat :: (Stream s) => Rule -> Parse s Texprs
flat g = do
  ts <- parse g
  pure $ maybeToList (flatten ts)

asUnit :: (Stream s) => Rule -> Parse s Texprs
asUnit g = parse g `mapErr` \err -> err{prior = []}

expect :: (Stream s) => Rule -> Text -> Parse s Texprs
expect g msg = do
  inp0 <- getInput
  parse g `mapErr` \err -> Err
    { prior = []
    , remaining = inp0
    , reason = (noReason $ location inp0)
                {expectingByName = Map.singleton msg err.reason}
    }

call :: (Stream s) => RuleName -> [Rule] -> Parse s Texprs
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

capture :: (Stream s) => ParamName -> Rule -> Rule -> Parse s Texprs
capture x g1 g2 = do
  ts1 <- parse g1
  let action' = withCapture x (mconcat $ unparse <$> ts1) (parse g2)
  ts2 <- action' `mapErr` \err -> err{prior = ts1 <> err.prior}
  pure $ ts1 <> ts2

replay :: (Stream s) => ParamName -> Parse s Texprs
replay x = lookupCapture x >>= \case
  Just str -> string str
  Nothing -> errorWithoutStackTrace  $ "internal Texpr-Peg error: unbound capture " ++ show x

combo :: (Stream s) => CtorName -> Parse s Texprs
combo name = Parse $ \env inp -> case takeTexpr inp of
  Just (t@(Combo _ name' _), inp') | name == name' -> This ([t], inp')
  _ ->
    let explain = (noReason $ location inp){expectingCtors = Set.singleton name}
     in unParse (throw explain) env inp

subst :: (Stream s) => Rule -> Parse s Rule
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
  Expect g msg -> flip Expect msg <$> subst g
  Call f [] -> lookupLocal f >>= \case
    Just g -> pure g
    Nothing -> pure $ Call f []
  Call f gs -> Call f <$> mapM subst gs
  Capture x g1 g2 -> Capture x <$> subst g1 <*> subst g2
  Replay x -> lookupCapture x >>= \case
    Just txt -> pure $ Str txt
    Nothing -> errorWithoutStackTrace  $ "internal Texpr-Peg error: unbound capture " ++ show x
  TexprCombo name -> pure $ TexprCombo name
