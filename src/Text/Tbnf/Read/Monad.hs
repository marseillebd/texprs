{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Text.Tbnf.Read.Monad
  ( Parse(..)
  , Stream(..)
  , Env(..)
  , throw
  , catch
  , mapErr
  , getInput
  , restoringInput
  , getPosition
  , withRange
  , withCall
  , withCapture
  , lookupGlobal
  , lookupLocal
  , lookupCapture
  , ReaderError(..)
  , noReason
  ) where

import Data.Bifunctor (first,second)
import Data.CharSet (CharSet)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Set (Set)
import Data.Texpr (Texpr,CtorName)
import Data.Text (Text)
import Data.These (These(..))
import Text.Location (Position,FwdRange,fwd)
import Text.Tbnf.Tree (Rule(..),RuleName,ParamName,paramNameFromRuleName)

import qualified Data.CharSet as CS
import qualified Data.Map as Map
import qualified Data.Set as Set

------ The Monad ------

newtype Parse s a = Parse { unParse :: Env -> s -> These (a, s) ReaderError }

data Env = Env
  { global :: Map RuleName ([ParamName], Rule)
  , local :: Map ParamName Rule
  , captures :: Map ParamName Text
  }
  deriving (Show)

instance Functor (Parse s) where
  fmap f (Parse action) = Parse $ \env inp -> first (first f) $ action env inp

instance (Stream s) => Applicative (Parse s) where
  pure x = Parse $ \_ inp -> This (x, inp)
  Parse action <*> Parse action' = Parse $ \env inp -> case action env inp of
    This (f, inp') -> first (first f) $ action' env inp'
    That err -> That err
    These (f, inp') err -> case action' env inp' of
      This ok -> These (first f ok) err
      That err' -> That (err <> err')
      These ok err' -> These (first f ok) (err <> err')

instance (Stream s) => Monad (Parse s) where
  Parse action >>= k = Parse $ \env inp -> case action env inp of
    This (x, inp') -> unParse (k x) env inp'
    That err -> That err
    These (x, inp') err -> case unParse (k x) env inp' of
      This ok -> These ok err
      That err' -> That (err <> err')
      These ok err' -> These ok (err <> err')


catch :: (Stream s) => Parse s a -> Parse s (Either ReaderError a)
catch action = Parse $ \env inp -> case unParse action env inp of
  This (ok, inp') -> This (Right ok, inp')
  That err -> These (Left err, inp) err
  These (ok, inp') err -> These (Right ok, inp') err

mapErr :: (Stream s) => Parse s a -> (ReaderError -> ReaderError) -> Parse s a
mapErr action f = Parse $ \env inp -> second f $ unParse action env inp

getInput :: (Stream s) => Parse s s
getInput = Parse $ \_ inp -> This (inp, inp)

restoringInput :: (Stream s) => Parse s a -> Parse s a
restoringInput action = Parse $ \env inp -> case unParse action env inp of
  This (ok, _) -> This (ok, inp)
  That err -> That err
  These (ok, _) err -> These (ok, inp) err

getPosition :: (Stream s) => Parse s Position
getPosition = Parse $ \_ inp -> This (location inp, inp)

withRange :: (Stream s) => Parse s a -> Parse s (FwdRange, a)
withRange action = do
  p0 <- getPosition
  x <- action
  p' <- getPosition
  pure (fwd p0 p', x)

throw :: (Stream s) => ReaderError -> Parse s a
throw reason = Parse $ \_ _ -> That reason

withCall :: (Stream s) => Map ParamName Rule -> Parse s a -> Parse s a
withCall local action = Parse $ \env inp -> unParse action env{local,captures=Map.empty} inp

withCapture :: (Stream s) => ParamName -> Text -> Parse s a -> Parse s a
withCapture x v action = Parse $ \env inp ->
  let env' = env{captures = Map.insert x v env.captures}
   in unParse action env' inp

lookupGlobal :: (Stream s) => RuleName -> Parse s (Maybe ([ParamName], Rule))
lookupGlobal x = Parse $ \env inp -> This (Map.lookup x env.global, inp)

lookupLocal :: (Stream s) => RuleName -> Parse s (Maybe Rule)
lookupLocal x0 = case paramNameFromRuleName x0 of
  Nothing -> pure Nothing
  Just x -> Parse $ \env inp -> This (Map.lookup x env.local, inp)

lookupCapture :: (Stream s) => ParamName -> Parse s (Maybe Text)
lookupCapture x = Parse $ \env inp -> This (Map.lookup x env.captures, inp)


------------------ Stream ------------------

-- | 'Text.Tbnf.Read.Generic.runReader' already exhibits parametric polymorphism
-- through most of its algorithm.
-- However, a few of the most primitive readers require inpection of the input
-- stream which will depend on the specifics of its type.
-- Implementing this class will provide 'Text.Tbnf.Read.Generic.runReader' with
-- the ability to inpect the input stream as necessary.
class Stream s where
  -- | Input streams must be able to report current 'Position' through this method.
  location :: s -> Position
  -- | If the given input stream begins with a character that is in the
  -- given 'CharSet', return that character and the rest of the input stream.
  takeChar :: CharSet -> s -> Maybe (Char, s)
  -- | Take as many characters as possible off the beginning of the input as
  -- long as those characters are all in the given 'CharSet' stream; return
  -- them and the remaining input stream.
  takeChars :: CharSet -> s -> (Text, s)
  -- | If the input stream begins with the given string, return the remaining
  -- input after that prefix.
  stripStringPrefix :: Text -> s -> Maybe s
  -- | If the input stream begins with a 'Texpr', return it along with the
  -- remaining input stream.
  takeTexpr :: s -> Maybe (Texpr, s)
  -- | Return whether the given input stream is empty
  isAtEnd :: s -> Bool

------------------ Errors ------------------

-- | Explanation for why a 'ReaderError' occurred.
-- I.e. what input was expected or unexpected at what position?
--
-- The 'Semigroup' for 'ReaderError' prefers reasons that are further in the input.
data ReaderError = ReaderError
  { expectAt :: Position
  , expectingEndOfInput :: Bool
  , expectingChars :: CharSet
  , expectingKeywords :: Set Text
  , expectingCtors :: Set CtorName
  , expectingByName :: Map Text ReaderError
  , unexpected :: Set Text
  }

-- | Give no explanation for why an error occurred.
-- This is used as a base value that can be modified to add expected/unexpected input.
noReason :: Position -> ReaderError
noReason expectAt = ReaderError
  { expectAt
  , expectingEndOfInput = False
  , expectingChars = CS.empty
  , expectingKeywords = Set.empty
  , expectingCtors = Set.empty
  , expectingByName = Map.empty
  , unexpected = Set.empty
  }

instance Show ReaderError where
  show r = concat
    [ "(noReason "
    , show r.expectAt
    , "){"
    , intercalate "," $ concat
      [ if r.expectingEndOfInput then ["expectingEndOfInput=True"] else []
      , if CS.null r.expectingChars then [] else ["expectingChars=" <> show r.expectingChars]
      , if Set.null r.expectingKeywords then [] else ["expectingKeywords=" <> show r.expectingKeywords]
      , if Set.null r.expectingCtors then [] else ["expectingCtors=" <> show r.expectingCtors]
      , if Map.null r.expectingByName then [] else ["expectingByName=" <> show r.expectingByName]
      ]
    , "}"
    ]

instance Semigroup ReaderError where
  a <> b = case a.expectAt `compare` b.expectAt of
    GT -> a
    EQ -> ReaderError
      { expectAt = a.expectAt
      , expectingEndOfInput = a.expectingEndOfInput || b.expectingEndOfInput
      , expectingChars = a.expectingChars <> b.expectingChars
      , expectingKeywords = a.expectingKeywords <> b.expectingKeywords
      , expectingCtors = a.expectingCtors <> b.expectingCtors
      , expectingByName = Map.unionWith (<>) a.expectingByName b.expectingByName
      , unexpected = a.unexpected <> b.unexpected
      }
    LT -> b
