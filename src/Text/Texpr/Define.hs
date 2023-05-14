{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Texpr.Define
  ( Peg(..)
  , StartDef
  , RuleDef
  , Rule(..)
  , SatClass(..)
  , ClassDef
  , CharClass(..)
  ) where

import Data.Texpr (CtorName)
import Text.Location (FwdRange)
import Text.Texpr.Tree (RuleName,ParamName)

data Peg = Peg
  { start :: StartDef
  , rules :: [RuleDef]
  , classes :: [ClassDef]
  }
  deriving (Show)

type StartDef = Maybe (FwdRange, RuleName)

type RuleDef = (FwdRange, (FwdRange, RuleName, [ParamName]), Rule)

data Rule
  = Alt FwdRange [Rule]
  | Seq FwdRange [Rule]
  | Cap FwdRange ParamName Rule Rule
  | Rep FwdRange Rule (Int, Maybe Int)
  | Sat FwdRange [SatClass]
  | SatNeg FwdRange [SatClass]
  | Char FwdRange Char
  | Str FwdRange String
  | End FwdRange
  | Void FwdRange String
  | Flat FwdRange Rule
  | Call FwdRange RuleName [Rule]
  | Ctor FwdRange CtorName Rule
  deriving (Show)

data SatClass
  = SatVar FwdRange RuleName
  | SatRange FwdRange Char Char
  | SatChar FwdRange Char
  | SatSet FwdRange [Char]
  deriving (Show)

type ClassDef = (FwdRange, (FwdRange, RuleName), CharClass)

data CharClass
  = ClassVar FwdRange RuleName
  | ClassRange FwdRange Char Char
  | ClassChar FwdRange Char
  | ClassSet FwdRange [Char]
  | ClassUnion CharClass CharClass
  | ClassMinus CharClass CharClass
  deriving (Show)
