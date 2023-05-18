{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Tbnf.Define
  ( Tbnf(..)
  , StartDef
  , RuleDef
  , Rule(..)
  , SatClass(..)
  , ClassDef
  , CharClass(..)
  ) where

import Data.Texpr (CtorName)
import Text.Location (FwdRange)
import Text.Tbnf.Tree (RuleName,ParamName)

-- | The abstract syntax of Tbnf grammars.
-- This is prior to analysis by a frontend.
data Tbnf = Tbnf
  { start :: StartDef
  , rules :: [RuleDef]
  , classes :: [ClassDef]
  }
  deriving (Show)

-- | The start definition, if it exists, simply names a nullary rule.
  -- If one is not given, then the default is to begin with the @start@ rule.
type StartDef = Maybe (FwdRange, RuleName)

-- | A rule definition consists of a rule name, list of parameters, and the rule
-- body.
type RuleDef = (FwdRange, (FwdRange, RuleName, [ParamName]), Rule)

-- | The abstract syntax of rule bodies.
data Rule
  = Alt FwdRange [Rule] -- ^ alternation (the @|@ operator)
  | Seq FwdRange [Rule] -- ^ sequencing
  | Cap FwdRange ParamName Rule Rule -- ^ capture the input matching the first rule, referencable by name within the second
  | Rep FwdRange Rule (Int, Maybe Int) -- ^ repedition operators with lower and upper bounds
  | Sat FwdRange [SatClass] -- ^ match a single character when it is in a given class
  | SatNeg FwdRange [SatClass] -- ^ match a single character when it is _not_ in a given class
  | Char FwdRange Char -- ^ match a single, specific character
  | Str FwdRange String -- ^ match a specific string (a sequence of specific characters)
  | End FwdRange -- ^ match at the end of input
  | Void FwdRange String -- ^ always fail to match, reporting the given error message
  | Expect FwdRange Rule String -- ^ if the rule fails to match, replace the error message
  | Flat FwdRange Rule -- ^ whatever texprs are formed by the rule matching, flatten the result into a single atom
  | Call FwdRange RuleName [Rule] -- ^ call a rule, use a parameter, or replay a named capture
  | Ctor FwdRange CtorName Rule -- ^ whatever texprs match the rule, package them into a combination with the given name
  | TexprCombo FwdRange CtorName -- ^ match a single texpr which is a combo with the given constructor
  deriving (Show)

-- | The abstract syntax defining character sets for the 'Sat' and 'SatNeg' constructors.
-- These define sets of characters.
data SatClass
  = SatVar FwdRange RuleName -- ^ include characters from the named character class
  | SatRange FwdRange Char Char -- ^ include characters that are between the lower and upper bounds (inclusive)
  | SatChar FwdRange Char -- ^ include the given specific character
  | SatSet FwdRange [Char] -- ^ include all of the given specific characters
  deriving (Show)

-- | Character class definitions consist of giving a name to a set of characters
type ClassDef = (FwdRange, (FwdRange, RuleName), CharClass)

-- | Defines a set of characters; see 'ClassDef'.
data CharClass
  = ClassVar FwdRange RuleName -- ^ the set of characters defined by name
  | ClassRange FwdRange Char Char -- ^ the set of characters between the lower and upper bounds (inclusive)
  | ClassChar FwdRange Char -- ^ a singleton set containing the given character
  | ClassSet FwdRange [Char] -- ^ a set containing exactly the given characters
  | ClassUnion CharClass CharClass -- ^ the union of two sets of characters
  | ClassMinus CharClass CharClass -- ^ those characters in the first set that do not appear in the second
  deriving (Show)
