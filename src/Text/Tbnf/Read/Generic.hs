{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , noReason
  , Stream(..)
  ) where

import {-# SOURCE #-} qualified Text.Tbnf.Read.Texpr as Texpr
import {-# SOURCE #-} qualified Text.Tbnf.Read.Text as Text

import Prelude hiding (any,fail,sequence)

import Text.Tbnf.Read.Monad

import Data.CharSet (CharSet)
import Data.Maybe (maybeToList)
import Data.Texpr (Texprs,Texpr(..),flatten,unparse,CtorName)
import Data.Text (Text)
import Data.These (These(..))
import Text.Location (fwd)
import Text.Tbnf.Tree (CompiledTbnf(..),Rule(..),RuleName,ParamName)

import qualified Data.CharSet as CS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Create a stream of 'Texpr's from an input stream that matches the given
-- grammar, or report an error.
runReader :: (Stream s)
  => CompiledTbnf
  -> s -- ^ input
  -> Either ReaderError (Texprs, s) -- ^ result with remaining input
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
  Any -> any
  Sat cs -> satisfy cs
  Many cs -> many cs
  Str str -> string str
  End -> end
  Void msg -> void msg
  Alt2 g1 g2 -> alternate g1 g2
  Empty -> pure []
  Seq2 g1 g2 -> sequence g1 g2
  Star g -> star g
  Lookahead g -> lookahead g
  NegLookahead msg g -> negLookahead msg g
  Ctor name g -> ctor name g
  Flat g -> flat g
  Expect g msg -> expect g msg
  Call f gs -> call f gs
  Capture x g1 g2 -> capture x g1 g2
  Replay x -> replay x
  TexprAtom g -> atom g
  TexprCombo name g -> combo name g

any :: (Stream s) => Parse s Texprs
any = Parse $ \_ inp -> case (takeChar CS.any inp, takeTexpr inp) of
  (Just (c, inp'), _) ->
    let loc = fwd (location inp) (location inp')
     in This ([Atom loc (T.singleton c)], inp')
  (_, Just (t, inp')) -> This ([t], inp')
  _ -> That explain
    where
    explain = (noReason $ location inp){unexpected = Set.singleton "end of input"}

satisfy :: (Stream s) => CharSet -> Parse s Texprs
satisfy cs = Parse $ \_ inp -> case takeChar cs inp of
  Just (c, inp') ->
    let loc = fwd (location inp) (location inp')
     in This ([Atom loc (T.singleton c)], inp')
  Nothing -> That explain
    where explain = (noReason $ location inp){expectingChars = cs} -- WARNING I'm assuming cs is a non-empty set

many :: (Stream s) => CharSet -> Parse s Texprs
many cs = Parse $ \_ inp ->
  let (txt, inp') = takeChars cs inp
      loc = fwd (location inp) (location inp')
   in This ([Atom loc txt], inp')

string :: (Stream s) => Text -> Parse s Texprs
string str = Parse $ \_ inp -> case stripStringPrefix str inp of
  Just inp' ->
    let loc = fwd (location inp) (location inp')
     in This ([Atom loc str], inp')
  Nothing -> That explain
    where explain = (noReason $ location inp){expectingKeywords = Set.singleton str}

end :: (Stream s) => Parse s Texprs
end = Parse $ \_ inp -> case isAtEnd inp of
  True -> This ([], inp)
  False -> That explain
    where explain = (noReason $ location inp){expectingEndOfInput = True}

void :: (Stream s) => Text -> Parse s Texprs
void msg = Parse $ \_ inp ->
  That (noReason $ location inp){unexpected = Set.singleton msg}

alternate :: (Stream s) => Rule -> Rule -> Parse s Texprs
alternate g1 g2 = do
  catch (parse g1) >>= \case
    Right ok -> pure ok
    Left err1 -> parse g2 `mapErr` (err1 <>)

sequence :: (Stream s) => Rule -> Rule -> Parse s Texprs
sequence g1 g2 = (<>) <$> parse g1 <*> parse g2

star :: (Stream s) => Rule -> Parse s Texprs
star g = do
  inp0 <- getInput
  catch (parse g) >>= \case
    Left _ -> pure []
    Right ts -> do
      inp' <- getInput
      if location inp' == location inp0
        then pure [] -- to prevent infinite loops when the repeated grammar accepts empty
        else (ts <>) <$> star g

lookahead :: (Stream s) => Rule -> Parse s Texprs
lookahead g = restoringInput $ [] <$ parse g

negLookahead :: (Stream s) => Text -> Rule -> Parse s Texprs
negLookahead msg g = do
  catch (restoringInput $ parse g) >>= \case
    Left _ -> pure []
    Right _ -> do
      inp <- getInput
      throw (noReason $ location inp){unexpected = Set.singleton msg}

ctor :: (Stream s) => CtorName -> Rule -> Parse s Texprs
ctor name g = do
  (r, ts) <- withRange $ parse g
  pure [Combo r name ts]

flat :: (Stream s) => Rule -> Parse s Texprs
flat g = do
  ts <- parse g
  pure $ maybeToList (flatten ts)

expect :: (Stream s) => Rule -> Text -> Parse s Texprs
expect g msg = do
  inp0 <- getInput
  parse g `mapErr` \err ->
    (noReason $ location inp0){expectingByName = Map.singleton msg err}

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
  ts2 <- action'
  pure $ ts1 <> ts2

replay :: (Stream s) => ParamName -> Parse s Texprs
replay x = lookupCapture x >>= \case
  Just str -> string str
  Nothing -> errorWithoutStackTrace  $ "internal Texpr-Peg error: unbound capture " ++ show x

atom :: (Stream s) => Maybe Rule -> Parse s Texprs
atom g_m = Parse $ \env inp -> case takeTexpr inp of
  Just (t@(Atom l txt), inp') -> case g_m of
    Nothing -> This ([t], inp')
    Just g -> case wrapParser g env (Text.Input l.anchor txt) of
      This ok -> This (ok, inp')
      That err -> That err
      These ok err -> These (ok, inp') err
  _ -> That (noReason $ location inp){expectingAtom = True}
  where
  wrapParser :: Rule -> Env -> Text.Input -> These Texprs ReaderError
  wrapParser g env txt = case unParse (parse g) env txt of
    This (ok, inp') -> case T.uncons inp'.txt of
      Nothing -> This ok
      Just (c, _) -> That $ moreCharsErr c inp'.loc
    That err -> That err
    These (ok, inp') err -> case T.uncons inp'.txt of
      Nothing -> This ok
      Just (c, _) -> That $ err <> moreCharsErr c inp'.loc
  moreCharsErr c loc = (noReason loc)
    { expectingByName = Map.singleton "end of atom" $
      (noReason loc){unexpected = Set.singleton $ T.singleton c}
    }

combo :: (Stream s) => CtorName -> Maybe Rule -> Parse s Texprs
combo name g_m = Parse $ \env inp -> case takeTexpr inp of
  Just (t@(Combo l name' children), inp') | name == name' -> case g_m of
    Nothing -> This ([t], inp')
    Just g -> case wrapParser g env (Texpr.Input l.anchor children) of
      This ok -> This ([Combo l name' ok], inp')
      That err -> That err
      These ok err -> These ([Combo l name' ok], inp') err
  _ -> That (noReason $ location inp){expectingCtors = Set.singleton name}
  where
  wrapParser g env children = case unParse (parse g) env children of
    This (ok, Texpr.Input{Texpr.toks=[]}) -> This ok
    This (_, Texpr.Input{Texpr.loc}) -> That $ moreChildrenErr loc
    That err -> That err
    These (ok, Texpr.Input{Texpr.toks=[]}) err -> These ok err
    These (_, Texpr.Input{Texpr.loc}) err -> That $ err <> moreChildrenErr loc
  moreChildrenErr loc = (noReason loc)
    { expectingByName = Map.singleton "end of children" $
      (noReason loc){expectingEndOfInput = True}
    }

subst :: (Stream s) => Rule -> Parse s Rule
subst = \case
  Any -> pure Any
  Sat cs -> pure $ Sat cs
  Many cs -> pure $ Many cs
  Str txt -> pure $ Str txt
  End -> pure End
  Void msg -> pure $ Void msg
  Alt2 g1 g2 -> Alt2 <$> subst g1 <*> subst g2
  Empty -> pure Empty
  Seq2 g1 g2 -> Seq2 <$> subst g1 <*> subst g2
  Star g -> Star <$> subst g
  Lookahead g -> Lookahead <$> subst g
  NegLookahead msg g -> NegLookahead msg <$> subst g
  Ctor name g -> Ctor name <$> subst g
  Flat g -> Flat <$> subst g
  Expect g msg -> flip Expect msg <$> subst g
  Call f [] -> lookupLocal f >>= \case
    Just g -> pure g
    Nothing -> pure $ Call f []
  Call f gs -> Call f <$> mapM subst gs
  Capture x g1 g2 -> Capture x <$> subst g1 <*> subst g2
  Replay x -> lookupCapture x >>= \case
    Just txt -> pure $ Str txt
    Nothing -> errorWithoutStackTrace  $ "internal Texpr-Peg error: unbound capture " ++ show x
  TexprAtom g -> TexprAtom <$> mapM subst g
  TexprCombo name g -> TexprCombo name <$> (mapM subst g)
