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

import Text.Location (FwdRange)

data Peg = Peg
  { start :: StartDef
  , rules :: [RuleDef]
  , classes :: [ClassDef]
  }
  deriving (Show)

type StartDef = Maybe (FwdRange, String)

type RuleDef = (FwdRange, (FwdRange, String, [String]), Rule)

data Rule
  = Alt FwdRange [Rule]
  | Seq FwdRange [Rule]
  | Cap FwdRange String Rule Rule
  | Rep FwdRange Rule (Int, Maybe Int)
  | Sat FwdRange [SatClass]
  | SatNeg FwdRange [SatClass]
  | Char FwdRange Char
  | Str FwdRange String
  | End FwdRange
  | Void FwdRange String
  | Flat FwdRange Rule
  | Call FwdRange String [Rule]
  | Ctor FwdRange String Rule
  deriving (Show)

data SatClass
  = SatVar FwdRange String
  | SatRange FwdRange Char Char
  | SatChar FwdRange Char
  | SatSet FwdRange [Char]
  deriving (Show)

type ClassDef = (FwdRange, (FwdRange, String), CharClass)

data CharClass
  = ClassVar FwdRange String
  | ClassRange FwdRange Char Char
  | ClassChar FwdRange Char
  | ClassSet FwdRange [Char]
  | ClassUnion CharClass CharClass
  | ClassMinus CharClass CharClass
  deriving (Show)
