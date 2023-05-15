-- | This module defines the abstract syntax of TBNF and a compiler taking it to
-- an internal representation suitable for use in reading.
--
-- TBNF stands for T-expr Backus-Naur Form.
-- It is a method of specifying grammars that produce 'Data.Texpr.Texpr'
-- streams inspired by Backus-Naur Form.
-- The main differences (beyond the concrete syntax) are:
--
-- - it is based on parser-expression grammars rather than general context-free grammars
--   (i.e. ordered choice and repetition extends as far as possible)
-- - it allows named capture of input to be replayed as a string match later
-- - facilities to construct texpr combinations
-- - facilities to match against texprs (in the case of separate lexper/parser stages)
--
-- While one could theoretically produce a 'Tbnf' value with Haskell source
-- code, it is much easier to produce one using the grammars defined
-- in "Text.Tbnf.Bootstrap" to parse TBNF source code.
-- The grammar defined there is generated from the TBNF grammar @docs/tbnf.tbnf@
-- in the source tarball; check it out for an example and documentation of the
-- TBNF syntax.
-- The exports of abstract syntax here are mainly for inspection rather than
-- construction.
--
-- To use the 'CompiledTbnf' grammars, one must use modules such as
-- "Text.Tbnf.Read.String", "Text.Tbnf.Read.Texpr", and so on.
-- If you have a new type of input stream you would like to parse,
-- see "Text.Tbnf.Read.Generic" for tools and documentation.
module Text.Tbnf
  ( -- * Compile TBNF Grammars
    compile
  , CompileError(..)
  , CompiledTbnf
  -- * TBNF Abstract Syntax
  , Tbnf(..)
  , StartDef
  , RuleDef
  , Rule(..)
  , SatClass(..)
  , ClassDef
  , CharClass(..)
  -- ** Identifier Newtypes
  , RuleName
  , ruleNameFromString
  , ruleNameToString
  , ParamName
  , paramNameFromString
  , paramNameToString
  , CtorName
  , ctorNameFromString
  , ctorNameToString
  -- ** Ranges
  , FwdRange
  , fwd
  ) where

import Data.Texpr (CtorName,ctorNameFromString,ctorNameToString)
import Text.Location (FwdRange,fwd)
import Text.Tbnf.Compile (compile,CompileError(..))
import Text.Tbnf.Define (SatClass(..), ClassDef, CharClass(..))
import Text.Tbnf.Define (Tbnf(..), StartDef, RuleDef, Rule(..))
import Text.Tbnf.Tree (ParamName,paramNameFromString,paramNameToString)
import Text.Tbnf.Tree (CompiledTbnf,RuleName,ruleNameFromString,ruleNameToString)
