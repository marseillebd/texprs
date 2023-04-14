{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Texpr.Monad where

import Prelude hiding (fail,sequence)

import Data.CharSet (CharSet)
import Data.List (isPrefixOf,stripPrefix)
import Data.Map (Map)
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Data.Texpr (Texprs,Texpr(..),Reason(..),noReason,flatten,unparse)
import Text.Location (Input(..),Source(..),Position(..),FwdRange,fwd)
import Text.Texpr.Tree (Rule(..))

import qualified Data.CharSet as CS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Texpr as Texpr
import qualified Text.Location as Loc

runPeg :: Map String ([String], Rule) -> Rule -> Input -> Either ErrorReport (Texprs, Input)
runPeg ruleSet startRule inp0 = case go of
  Right (a, inp) -> Right (a, inp)
  Left err -> Left err
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
  Void -> void
  Alt2 g1 g2 -> alternate g1 g2
  Empty -> pure []
  Seq2 g1 g2 -> sequence g1 g2
  Star2 g1 g2 -> star g1 g2
  Ctor name g -> ctor name g
  Flat g -> flat g
  AsUnit g -> asUnit g
  Expect desc g -> expect desc g
  -- Fail msg -> fail msg
  Call f gs -> call f gs
  Capture x g1 g2 -> capture x g1 g2
  Replay x -> replay x
  Recover g1 g2 -> recover g1 g2

satisfy :: CharSet -> Parse Texprs
satisfy cs = Parse $ \env inp -> case inp.txt of
  c:txt' | c `CS.elem` cs -> Right ([t], Input loc' txt')
    where
    t = Atom (fwd inp.loc loc') (c:"")
    loc' = Loc.advance inp.loc [c]
  _ -> unParse (throw explain) env inp -- TODO can I get these explains inlined?
    where explain = (noReason inp.loc){expectingChars = cs} -- WARNING I'm assuming cs is a non-empty set

many :: CharSet -> Parse Texprs
many cs = Parse $ \_ inp -> case span (`CS.elem` cs) inp.txt of
  (ok, txt') -> Right ([t], Input loc' txt')
    where
    t = Atom (fwd inp.loc loc') ok
    loc' = Loc.advance inp.loc ok

string :: String -> Parse Texprs
string "" = Parse $ \_ inp -> Right ([], inp)
string str = Parse $ \env inp -> case stripPrefix str inp.txt of
  Just txt' -> Right ([t], Input loc' txt')
    where
    t = Atom (fwd inp.loc loc') str
    loc' = Loc.advance inp.loc str
  Nothing -> unParse (throw explain) env inp
    where explain = (noReason inp.loc){expectingKeywords = Set.singleton str}

end :: Parse Texprs
end = Parse $ \env inp -> case inp.txt of
  "" -> Right ([], inp)
  _ -> unParse (throw explain) env inp
    where explain = (noReason inp.loc){expectingEndOfInput = True}

void :: Parse Texprs
void = errorWithoutStackTrace "TODO: void"
-- void = Parse $ \env inp -> case inp.txt of
--   "" -> Right ([], inp)
--   _ -> unParse (throw explain) env inp
--     where explain = (noReason inp.loc){expectingEndOfInput = True}

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

star :: Rule -> Rule -> Parse Texprs
star g1 g2 = do
  inp0 <- getInput
  catch (parse g1) >>= \case
    Left _ -> pure []
    Right ts1 -> catch (parse g2) >>= \case
      Left err
        -- WARNING wow, this is the one place I use setInput, and I just find it gross
        | null err.prior -> setInput inp0 >> pure []
        | otherwise -> rethrow err{prior = ts1 <> err.prior}
      Right ((ts1 <>) -> ts)
        | null ts -> pure [] -- to prevent infinite loops when the repeated grammar accepts empty
        | otherwise -> (ts <>) <$> mapErr (star g1 g2) (\err -> err{prior = ts <> err.prior})

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
  Void -> pure Void
  Alt2 g1 g2 -> Alt2 <$> subst g1 <*> subst g2
  Empty -> pure Empty
  Seq2 g1 g2 -> Seq2 <$> subst g1 <*> subst g2
  Star2 g1 g2 -> Star2 <$> subst g1 <*> subst g2
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
  Recover g1 g2 -> Recover <$> subst g1 <*> subst g2

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

recover :: Rule -> Rule -> Parse Texprs
recover g1 g2 = catch (parse g1) >>= \case
  Right ts1 -> catch (parse g2) >>= \case
    Right ts2 -> pure $ ts1 <> ts2
    Left err -> do
      (preErr, skip) <- performSkip err g2
      catch (parse g2) >>= \case
        Right ts2' -> pure $ ts1 <> err.prior <> [Error preErr err.reason skip] <> ts2'
        Left _ -> rethrow err{prior = ts1 <> err.prior}
  Left err -> do
    (preErr, skip) <- performSkip err g2
    catch (parse g2) >>= \case
      Right ts2 -> pure $ err.prior <> [Error preErr err.reason skip] <> ts2
      Left _ -> rethrow err

performSkip :: ErrorReport -> Rule -> Parse (Source, Source)
performSkip err g = Parse $ \env inp ->
  let tsLoc = if null err.prior then inp.loc else Texpr.end (last err.prior)
      remaining' = advanceTo (next g env) err.remaining
   in Right ( ( Loc.slice inp (fwd tsLoc err.remaining.loc)
              , Loc.slice err.remaining (fwd err.remaining.loc remaining'.loc)
              )
            , remaining'
            )

------ The Monad ------

newtype Parse a = Parse { unParse :: Env -> Input -> Either ErrorReport (a, Input) }

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
  fmap f (Parse action) = Parse $ \env inp -> case action env inp of
    Right (x, inp') -> Right (f x, inp')
    Left err -> Left err

instance Applicative Parse where
  pure x = Parse $ \_ inp -> Right (x, inp)
  Parse action <*> Parse action' = Parse $ \env inp -> case action env inp of
    Right (f, inp') -> case action' env inp' of
      Right (x, inp'') -> Right (f x, inp'')
      Left err -> Left err
    Left err -> Left err

instance Monad Parse where
  Parse action >>= k = Parse $ \env inp -> case action env inp of
    Right (x, inp') -> unParse (k x) env inp'
    Left err -> Left err

throw :: Reason -> Parse a
throw reason = Parse $ \_ remaining -> Left $
  Err { prior = [], reason, remaining }

rethrow :: ErrorReport -> Parse a
rethrow err = Parse $ \_ _ -> Left err

catch :: Parse a -> Parse (Either ErrorReport a)
catch action = Parse $ \env inp -> case unParse action env inp of
  Right (ok, inp') -> Right (Right ok, inp')
  Left err -> Right (Left err, inp)

mapErr :: Parse a -> (ErrorReport -> ErrorReport) -> Parse a
mapErr action f = Parse $ \env inp -> case unParse action env inp of
  Right ok -> Right ok
  Left err -> Left (f err)

getInput :: Parse Input
getInput = Parse $ \_ inp -> Right (inp, inp)

setInput :: Input -> Parse ()
setInput inp = Parse $ \_ _ -> Right ((), inp)

getPosition :: Parse Position
getPosition = Parse $ \_ inp -> Right (inp.loc, inp)

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
lookupGlobal x = Parse $ \env inp -> Right (Map.lookup x env.global, inp)

lookupLocal :: String -> Parse (Maybe Rule)
lookupLocal x = Parse $ \env inp -> Right (Map.lookup x env.local, inp)

lookupCapture :: String -> Parse (Maybe String)
lookupCapture x = Parse $ \env inp -> Right (Map.lookup x env.captures, inp)

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

nextChar :: Next -> CharSet
nextChar Next{acceptEmpty=True} = errorWithoutStackTrace "internal Texpr-Peg error: nextChar should not be called when next accepts empty"
nextChar Next{expectChars,expectStrs} =
  mconcat $ expectChars : (nextStrStart <$> Set.toList expectStrs)
  where
  nextStrStart "" = errorWithoutStackTrace "internal Texpr-Peg error: ExpectStrings should not contain the empty string"
  nextStrStart (c:_) = CS.singleton c

nextStrings :: Next -> [String]
nextStrings n = Set.toList n.expectStrs

-- return next expected inputs, and whether an empty string is acccepted
next :: Rule -> Env -> Next
next = \case
  Sat cs -> const $ Next
    { expectChars = cs
    , expectStrs = Set.empty
    , acceptEmpty = False
    }
  Many cs -> const $ Next
    { expectChars = cs
    , expectStrs = Set.empty
    , acceptEmpty = True
    }
  Str "" -> const $ mempty{acceptEmpty = True}
  Str str -> const $ Next
    { expectChars = CS.empty
    , expectStrs = Set.singleton str
    , acceptEmpty = False
    }
  End -> const $ mempty
  Void -> const $ mempty
  Alt2 g1 g2 -> \env -> next g1 env <> next g2 env
  Empty -> const $ mempty{acceptEmpty = True}
  Seq2 g1 g2 -> \env ->
    let n1 = next g1 env
     in if n1.acceptEmpty then n1 <> next g2 env else n1
  Star2 g1 g2 -> \env -> (next (Seq2 g1 g2) env){acceptEmpty = True}
  Ctor _ g -> next g
  Flat g -> next g
  AsUnit g -> next g
  Expect _ g -> next g
  -- Fail _ -> const mempty
  Call name args -> \env -> case Map.lookup name env.local of
    Just g -> next g env
    Nothing -> case Map.lookup name env.global of
      Just (params, g) ->
        let locals' = Map.fromList (zip params args)
         in next g env{local=locals',captures=Map.empty}
      Nothing -> mempty
  Capture _ g1 g2 -> next (Seq2 g1 g2)
  Replay name -> \env -> case Map.lookup name env.captures of
    Just txt -> next (Str txt) env
    Nothing -> mempty
  Recover g1 g2 -> next (Seq2 g1 g2)

advanceTo :: Next -> Input -> Input
advanceTo n inp0
  | n.acceptEmpty = inp0
  | otherwise = loop inp0
  where
  loop :: Input -> Input
  loop inp = case break (`CS.elem` nextChar n) inp.txt of
    (skip, "") -> inp' skip ""
    (skip, txt'@(c:rest))
      | c `CS.elem` n.expectChars -> inp' skip txt'
      | (`isPrefixOf` txt') `any` (nextStrings n) -> inp' skip txt'
      | otherwise -> loop (inp' (skip <> [c]) rest)
    where
    inp' :: String -> String -> Input
    inp' skip rest = Input (inp.loc `Loc.advance` skip) rest
