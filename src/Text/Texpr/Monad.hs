{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Text.Texpr.Monad
  ( Parse(..)
  , Stream(..)
  , Env(..)
  , throw
  , catch
  , mapErr
  , getInput
  , getPosition
  , withRange
  , withCall
  , withCapture
  , lookupGlobal
  , lookupLocal
  , lookupCapture
  , ErrorReport(..)
  , Reason(..)
  , noReason
  ) where

import Data.Bifunctor (first,second)
import Data.CharSet (CharSet)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Set (Set)
import Data.Texpr (Texprs,Texpr)
import Data.These (These(..))
import Text.Location (Position,FwdRange,fwd)
import Text.Texpr.Tree (Rule(..))

import qualified Data.CharSet as CS
import qualified Data.Map as Map
import qualified Data.Set as Set

------ The Monad ------

newtype Parse s a = Parse { unParse :: Env -> s -> These (a, s) (ErrorReport s) }

data Env = Env
  { global :: Map String ([String], Rule)
  , local :: Map String Rule
  , captures :: Map String String
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


catch :: (Stream s) => Parse s a -> Parse s (Either (ErrorReport s) a)
catch action = Parse $ \env inp -> case unParse action env inp of
  This (ok, inp') -> This (Right ok, inp')
  That err -> These (Left err, inp) err
  These (ok, inp') err -> These (Right ok, inp') err

mapErr :: (Stream s) => Parse s a -> (ErrorReport s -> ErrorReport s) -> Parse s a
mapErr action f = Parse $ \env inp -> second f $ unParse action env inp

getInput :: (Stream s) => Parse s s
getInput = Parse $ \_ inp -> This (inp, inp)

getPosition :: (Stream s) => Parse s Position
getPosition = Parse $ \_ inp -> This (location inp, inp)

withRange :: (Stream s) => Parse s a -> Parse s (FwdRange, a)
withRange action = do
  p0 <- getPosition
  x <- action
  p' <- getPosition
  pure (fwd p0 p', x)

throw :: (Stream s) => Reason -> Parse s a
throw reason = Parse $ \_ remaining -> That $
  Err { prior = [], reason, remaining }

withCall :: (Stream s) => Map String Rule -> Parse s a -> Parse s a
withCall local action = Parse $ \env inp -> unParse action env{local,captures=Map.empty} inp

withCapture :: (Stream s) => String -> String -> Parse s a -> Parse s a
withCapture x v action = Parse $ \env inp ->
  let env' = env{captures = Map.insert x v env.captures}
   in unParse action env' inp

lookupGlobal :: (Stream s) => String -> Parse s (Maybe ([String], Rule))
lookupGlobal x = Parse $ \env inp -> This (Map.lookup x env.global, inp)

lookupLocal :: (Stream s) => String -> Parse s (Maybe Rule)
lookupLocal x = Parse $ \env inp -> This (Map.lookup x env.local, inp)

lookupCapture :: (Stream s) => String -> Parse s (Maybe String)
lookupCapture x = Parse $ \env inp -> This (Map.lookup x env.captures, inp)


------------------ Stream ------------------

class Stream s where
  location :: s -> Position
  takeChar :: CharSet -> s -> Maybe (Char, s)
  takeChars :: CharSet -> s -> (String, s)
  stripStringPrefix :: String -> s -> Maybe s
  takeTexpr :: s -> Maybe (Texpr, s)
  isAtEnd :: s -> Bool

------------------ Errors ------------------

data ErrorReport s = Err
  { prior :: Texprs
  , reason :: Reason
  , remaining :: s
  }
  deriving (Show)

instance (Stream s) => Semigroup (ErrorReport s) where
  a <> b = case location a.remaining `compare` location b.remaining of
    GT -> a
    EQ -> a{reason = a.reason <> b.reason}
    LT -> b

data Reason = Reason
  { expectAt :: Position
  , expectingEndOfInput :: Bool
  , expectingChars :: CharSet
  , expectingKeywords :: Set String
  , expectingCtors :: Set String
  , expectingByName :: Map String Reason
  , unexpected :: Set String
  }

noReason :: Position -> Reason
noReason expectAt = Reason
  { expectAt
  , expectingEndOfInput = False
  , expectingChars = CS.empty
  , expectingKeywords = Set.empty
  , expectingCtors = Set.empty
  , expectingByName = Map.empty
  , unexpected = Set.empty
  }

instance Show Reason where
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

instance Semigroup Reason where
  a <> b = case a.expectAt `compare` b.expectAt of
    GT -> a
    EQ -> Reason
      { expectAt = a.expectAt
      , expectingEndOfInput = a.expectingEndOfInput || b.expectingEndOfInput
      , expectingChars = a.expectingChars <> b.expectingChars
      , expectingKeywords = a.expectingKeywords <> b.expectingKeywords
      , expectingCtors = a.expectingCtors <> b.expectingCtors
      , expectingByName = Map.unionWith (<>) a.expectingByName b.expectingByName
      , unexpected = a.unexpected <> b.unexpected
      }
    LT -> b
