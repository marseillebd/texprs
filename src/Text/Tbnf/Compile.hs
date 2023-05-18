{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Tbnf.Compile
  ( compile
  , CompileError(..)
  ) where

import Control.Monad (foldM)
import Data.CharSet (CharSet)
import Data.Graph (SCC(..),stronglyConnComp)
import Data.List (find)
import Data.Map (Map)
import Data.Set (Set)
import Text.Location (FwdRange)
import Text.Tbnf.Define (StartDef,RuleDef,ClassDef)
import Text.Tbnf.Define (Tbnf(..),Rule(..),SatClass(..),CharClass(..))
import Text.Tbnf.Tree (CompiledTbnf,RuleName,ParamName,paramNameFromRuleName,ruleNameFromParamName)

import qualified Data.CharSet as CS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Tbnf.Tree as Tree

-- | Represent any error that may occur during 'compile'.
data CompileError
  = CircularClassDefinitions [(FwdRange, RuleName)]
  -- ^ class definitions are not allowed to reference themselves in their definition
  -- (directly or indirectly through other class definitions)
  | DuplicateClassDefinitions [FwdRange] RuleName
  -- ^ two or more class definitions may not have the same name
  | DuplicateRuleDefinitions [FwdRange] RuleName
  -- ^ two or more rule definitions may not have the same name
  | EmptyRange FwdRange Char Char
  -- ^ where a character range is defined by lower and upper bounds,
  -- the lower bound must be less than or equal to the upper bound
  | EmptyRepetition FwdRange Int Int
  -- ^ in bounded repetition (such as @foo{1,4}@),
  -- the the lower bound must be less than or equal to the upper bound
  | NoStartRule
  -- ^ no start rule could be identified
  | ReplayTakesNoArguments FwdRange ParamName
  -- ^ variables bound to captures are nullary
  | StartRuleCannotBeParametric FwdRange RuleName [ParamName]
  -- ^ the start rule must be nullary
  | UndefinedClass FwdRange RuleName
  -- ^ a character class was referenced by name, but that name was undefined
  | UndefinedRule FwdRange RuleName
  -- ^ a rule was referenced by name, but that name was undefined
  | WrongNumberOfArguments FwdRange Int RuleName Int
  -- ^ a call to a (global) rule was made with a number of arguments that does not match that rule's arity
  deriving (Show)

-- | Perform frontend analysis to ensure the given grammar is well-formed.
-- TODO: optimize the resulting grammar.
compile :: Tbnf -> Either [CompileError] CompiledTbnf
compile peg = do
  clss <- compileClasses peg.classes
  rs <- compileRules clss peg.rules
  s <- compileStart peg.start peg.rules
  pure $ Tree.Tbnf rs s

------------------ Start Rule ------------------

compileStart :: StartDef -> [RuleDef] -> Either [CompileError] Tree.Rule
compileStart Nothing rules = case find isStart rules of
  Just (_, (_, g, []), _) -> pure $ Tree.Call g []
  Just (_, (r, g, params), _) ->Left [StartRuleCannotBeParametric r g params]
  Nothing -> Left [NoStartRule]
  where
  isStart (_, (_, "start", _), _) = True
  isStart _ = False
compileStart (Just (r, x)) gs = case find (\(_, (_, y, _), _) -> x == y) gs of
  Just (_, (_, g, []), _) -> pure $ Tree.Call g []
  Just (_, (r', g, params), _) -> Left [StartRuleCannotBeParametric r' g params]
  Nothing -> Left [UndefinedRule r x]

------------------ Rule Definitions ------------------

compileRules ::
     Map RuleName CharSet
  -> [RuleDef]
  -> Either [CompileError] (Map RuleName ([ParamName], Tree.Rule))
compileRules clss gs = do
  () <- checkDuplicateNames getName DuplicateRuleDefinitions gs
  let arities = Map.fromList $ getArityDef <$> gs
      st0 = RuleSt
        { classes = clss
        , arities
        , captures = Set.empty
        , isUnderFlat = False
        }
      go :: RuleDef -> Either [CompileError] (Map RuleName ([ParamName], Tree.Rule))
      go (_, (_, x, params), g) = do
        let locals = Map.fromList $ (,0) . ruleNameFromParamName <$> params
            st = st0{arities = locals `Map.union` st0.arities}
        g' <- compileRule st g
        pure $ Map.singleton x (params, g')
  mergeEithers (go <$> gs)
  where
  getName (_, (r, x, _), _) = (r, x)
  getArityDef (_, (_, x, params), _) = (x, length params)

data CompileRuleState = RuleSt
  { classes :: Map RuleName CharSet
  , arities :: Map RuleName Int
  , captures :: Set ParamName
  , isUnderFlat :: Bool
  }

compileRule :: CompileRuleState -> Rule -> Either [CompileError] Tree.Rule
compileRule st = \case
  Alt _ gs -> Tree.Alt <$> compileRule st `mapM` gs
  Seq _ gs -> Tree.Seq <$> compileRule st `mapM` gs
  Cap _ name capture scope ->
    let st' = st{captures = Set.insert name st.captures}
     in Tree.Capture name <$> compileRule st capture <*> compileRule st' scope
  Rep loc _ (lo, Just hi) | lo > hi -> Left [EmptyRepetition loc lo hi]
  Rep _ g (lo, hi) -> do
    g' <- compileRule st g
    pure $ compileRep g' lo hi
  Sat _ sats -> do
    cls <- mergeEithers $ compileSatisfy st.classes <$> sats
    pure $ Tree.Sat cls
  SatNeg _ sats -> do
    cls <- mergeEithers $ compileSatisfy st.classes <$> sats
    pure $ Tree.Sat (CS.complement cls)
  Char _ c -> pure $ Tree.Sat (CS.singleton c)
  Str _ "" -> pure $ Tree.Empty
  Str _ str -> pure $ Tree.Str str
  End _ -> pure Tree.End
  Void _ msg -> pure $ Tree.Void msg
  Expect _ g msg -> flip Tree.Expect msg <$> compileRule st g
  Flat _ g -> Tree.Flat <$> compileRule st g
  Call loc name args -> case Map.lookup name st.arities of
    Just ar
      | length args == ar -> Tree.Call name <$> compileRule st `mapM` args
      | otherwise -> Left [WrongNumberOfArguments loc ar name (length args)]
    Nothing -> case paramNameFromRuleName name of
      Just name' -> case name' `Set.member` st.captures of
        True
          | null args -> pure $ Tree.Replay name'
          | otherwise -> Left [ReplayTakesNoArguments loc name']
        False -> Left [UndefinedRule loc name]
      Nothing -> Left [UndefinedRule loc name]
  Ctor _ name g -> Tree.Ctor name <$> compileRule st g
  TexprCombo _ name -> pure $ Tree.TexprCombo name

compileRep :: Tree.Rule -> Int -> Maybe Int -> Tree.Rule
compileRep !g !n0 Nothing = loop n0
  where
  loop 0 = Tree.Star g
  loop !n = Tree.Seq [ g, loop (n - 1) ]
compileRep !g !n0 (Just !m0) = loop n0 m0
  where
  loop 0 0 = Tree.Empty
  loop 0 1 = g `Tree.Alt2` Tree.Empty
  loop 0 !m = (Tree.Seq [ g, loop 0 (m - 1) ]) `Tree.Alt2` Tree.Empty
  loop !n !m = Tree.Seq [ g, loop (n - 1) (m - 1) ]

compileSatisfy :: Map RuleName CharSet -> SatClass -> Either [CompileError] CharSet
compileSatisfy clss = \case
  SatVar loc x -> case Map.lookup x clss of
    Just cls -> pure cls
    Nothing -> Left [UndefinedClass loc x]
  SatRange loc lo hi
    | lo <= hi -> pure $ CS.contiguous lo hi
    | otherwise -> Left [EmptyRange loc lo hi]
  SatChar _ c -> pure $ CS.singleton c
  SatSet _ cs -> pure $ CS.oneOf cs

------------------ Character Classes ------------------

compileClasses :: [ClassDef] -> Either [CompileError] (Map RuleName CharSet)
compileClasses clss = do
  () <- checkDuplicateNames getName DuplicateClassDefinitions clss
  let go defd = \case
        CyclicSCC circClss -> Left [CircularClassDefinitions (toError <$> circClss)]
        AcyclicSCC cls -> Map.union defd <$> compileClass defd cls
  foldM go Map.empty (stronglyConnComp $ toVertex <$> clss)
  where
  getName (_, it, _) = it
  toVertex :: ClassDef -> (ClassDef, RuleName, [RuleName])
  toVertex def@(_, (_, name), body) = (def, name, fv body)
  toError :: ClassDef -> (FwdRange, RuleName)
  toError (_, nameLoc, _) = nameLoc
  fv :: CharClass -> [RuleName]
  fv (ClassVar _ x) = [x]
  fv (ClassRange _ _ _) = []
  fv (ClassChar _ _) = []
  fv (ClassSet _ _) = []
  fv (ClassUnion a b) = fv a <> fv b
  fv (ClassMinus a b) = fv a <> fv b

compileClass :: Map RuleName CharSet -> ClassDef -> Either [CompileError] (Map RuleName CharSet)
compileClass defd (_, (_, name), body) = Map.singleton name <$> go body
  where
  go :: CharClass -> Either [CompileError] CharSet
  go (ClassVar r x) = case Map.lookup x defd of
    Just v -> pure v
    Nothing -> Left [UndefinedClass r x]
  go (ClassRange r lo hi)
    | lo <= hi = pure $ CS.contiguous lo hi
    | otherwise = Left [EmptyRange r lo hi]
  go (ClassChar _ c) = pure $ CS.singleton c
  go (ClassSet _ cs) = pure $ CS.oneOf cs
  go (ClassUnion a b) = CS.union <$> go a <*> go b
  go (ClassMinus a b) = CS.minus <$> go a <*> go b

------------------ Helpers ------------------

checkDuplicateNames ::
     (a -> (FwdRange, RuleName)) -- ^ map a definition to its (located) name
  -> ([FwdRange] -> RuleName -> CompileError)
  -- ^ map a name and its multiple definition locations to an error
  -> [a]
  -> Either [CompileError] ()
checkDuplicateNames getName toError xs = case detect of
  [] -> Right ()
  errs -> Left errs
  where
  detect :: [CompileError]
  detect = fmap (uncurry $ flip toError) . Map.assocs
    $ Map.filter isMultipleDefinition
    $ Map.unionsWith (<>) (toName <$> xs)
  toName (getName -> (r, name)) = Map.singleton name [r]
  isMultipleDefinition [] = False
  isMultipleDefinition [_] = False
  isMultipleDefinition _ = True

mergeEither :: (Monoid a, Monoid b) => Either a b -> Either a b -> Either a b
mergeEither (Right xs) (Right ys) = Right (xs <> ys)
mergeEither (Left xs) (Right _) = Left xs
mergeEither (Right _) (Left ys) = Left ys
mergeEither (Left xs) (Left ys) = Left (xs <> ys)

mergeEithers :: (Monoid a, Monoid b) => [Either a b] -> Either a b
mergeEithers = foldr mergeEither (Right mempty)
