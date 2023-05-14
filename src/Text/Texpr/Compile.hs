{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Texpr.Compile
  ( compile
  ) where

import Control.Monad (foldM)
import Data.CharSet (CharSet)
import Data.Graph (SCC(..),stronglyConnComp)
import Data.List (find)
import Data.Map (Map)
import Data.Set (Set)
import Text.Location (FwdRange)
import Text.Texpr.Define (Peg(..),Rule(..),SatClass(..),CharClass(..))
import Text.Texpr.Define (StartDef,RuleDef,ClassDef)
import Text.Texpr.Tree (RuleName,ParamName,paramNameFromRuleName,ruleNameFromParamName)

import qualified Data.CharSet as CS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Texpr.Tree as Tree

data Error
  = CircularClassDefinitions [(FwdRange, RuleName)]
  | DuplicateClassDefinitions [FwdRange] RuleName
  | DuplicateRuleDefinitions [FwdRange] RuleName
  | EmptyRange FwdRange Char Char
  | EmptyRepetition FwdRange Int Int
  | NoStartRule
  | ReplayTakesNoArguments FwdRange ParamName
  | StartRuleCannotBeParametric FwdRange RuleName [ParamName]
  | UndefinedClass FwdRange RuleName
  | UndefinedRule FwdRange RuleName
  | WrongNumberOfArguments FwdRange Int RuleName Int
  deriving (Show)

compile :: Peg -> Either [Error] (Map RuleName ([ParamName], Tree.Rule), Tree.Rule)
compile peg = do
  clss <- compileClasses peg.classes
  rs <- compileRules clss peg.rules
  s <- compileStart peg.start peg.rules
  pure (rs, s)

------------------ Start Rule ------------------

compileStart :: StartDef -> [RuleDef] -> Either [Error] Tree.Rule
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
  -> Either [Error] (Map RuleName ([ParamName], Tree.Rule))
compileRules clss gs = do
  () <- checkDuplicateNames getName DuplicateRuleDefinitions gs
  let arities = Map.fromList $ getArityDef <$> gs
      st0 = RuleSt
        { classes = clss
        , arities
        , captures = Set.empty
        , isUnderFlat = False
        }
      go :: RuleDef -> Either [Error] (Map RuleName ([ParamName], Tree.Rule))
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

compileRule :: CompileRuleState -> Rule -> Either [Error] Tree.Rule
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

compileSatisfy :: Map RuleName CharSet -> SatClass -> Either [Error] CharSet
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

compileClasses :: [ClassDef] -> Either [Error] (Map RuleName CharSet)
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

compileClass :: Map RuleName CharSet -> ClassDef -> Either [Error] (Map RuleName CharSet)
compileClass defd (_, (_, name), body) = Map.singleton name <$> go body
  where
  go :: CharClass -> Either [Error] CharSet
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
  -> ([FwdRange] -> RuleName -> Error) -- ^ map a name and its multiple definition locations to an error
  -> [a]
  -> Either [Error] ()
checkDuplicateNames getName toError xs = case detect of
  [] -> Right ()
  errs -> Left errs
  where
  detect :: [Error]
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
