{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
import Text.Location (FwdRange)
import Text.Texpr.Define (Peg(..),Rule(..),SatClass(..),CharClass(..))
import Text.Texpr.Define (StartDef,RuleDef,ClassDef)

import qualified Data.CharSet as CS
import qualified Data.Map as Map
import qualified Text.Texpr.Tree as Tree

data Error
  = CircularClassDefinitions [(FwdRange, String)]
  | DuplicateClassDefinitions [FwdRange] String
  | DuplicateRuleDefinitions [FwdRange] String
  | EmptyGrammar
  | EmptyRange FwdRange Char Char
  | EmptyRepetition FwdRange Int Int
  | StartRuleCannotBeParametric FwdRange String [String]
  | UndefinedClass FwdRange String
  | UndefinedRule FwdRange String
  | WrongNumberOfArguments FwdRange Int String Int
  deriving (Show)

compile :: Peg -> Either [Error] (Map String ([String], Tree.Rule), Tree.Rule)
compile peg = do
  clss <- compileClasses peg.classes
  rs <- compileRules clss peg.rules
  s <- compileStart peg.start peg.rules
  pure (rs, s)

------------------ Start Rule ------------------

compileStart :: StartDef -> [RuleDef] -> Either [Error] Tree.Rule
compileStart Nothing [] = Left [EmptyGrammar]
compileStart Nothing ((_, (_, g, []), _):_) = pure $ Tree.Call g []
compileStart Nothing ((_, (r, g, params), _):_) = Left [StartRuleCannotBeParametric r g params]
compileStart (Just (r, x)) gs = case find (\(_, (_, y, _), _) -> x == y) gs of
  Just (_, (_, g, []), _) -> pure $ Tree.Call g []
  Just (_, (r', g, params), _) -> Left [StartRuleCannotBeParametric r' g params]
  Nothing -> Left [UndefinedRule r x]

------------------ Rule Definitions ------------------

compileRules :: Map String CharSet -> [RuleDef] -> Either [Error] (Map String ([String], Tree.Rule))
compileRules clss gs = do
  () <- checkDuplicateNames getName DuplicateRuleDefinitions gs
  let arities = Map.fromList $ getArityDef <$> gs
      st0 = RuleSt
        { classes = clss
        , arities
        , isUnderFlat = False
        }
      go :: RuleDef -> Either [Error] (Map String ([String], Tree.Rule))
      go (_, (_, x, params), g) = do
        let locals = Map.fromList $ (,0) <$> params
            st = st0{arities = locals `Map.union` st0.arities}
        g' <- compileRule st g
        pure $ Map.singleton x (params, g')
  mergeEithers (go <$> gs)
  where
  getName (_, (r, x, _), _) = (r, x)
  getArityDef (_, (_, x, params), _) = (x, length params)

data CompileRuleState = RuleSt
  { classes :: Map String CharSet
  , arities :: Map String Int
  , isUnderFlat :: Bool
  }

compileRule :: CompileRuleState -> Rule -> Either [Error] Tree.Rule
compileRule st = \case
  Alt _ gs -> Tree.Alt <$> compileRule st `mapM` gs
  Seq _ gs -> Tree.Seq <$> compileRule st `mapM` gs
  -- Rep FwdRange (Rule, Maybe Rule) (Int, Maybe Int)
  Rep loc _ (lo, Just hi) | lo > hi -> Left [EmptyRepetition loc lo hi]
  Rep _ (g1, g2) (lo, hi) -> do
    g1' <- compileRule st g1
    g2' <- maybe (pure Tree.Empty) (compileRule st) g2
    pure $ compileRep g1' g2' lo hi
  Sat _ sats -> do
    cls <- mergeEithers $ compileSatisfy st.classes <$> sats
    pure $ Tree.Sat cls
  SatNeg _ sats -> do
    cls <- mergeEithers $ compileSatisfy st.classes <$> sats
    pure $ Tree.Sat (CS.complement cls)
  Char _ c -> pure $ Tree.Sat (CS.singleton c)
  Str _ str -> pure $ Tree.Str str
  End _ -> pure Tree.End
  Void _ msg -> pure $ Tree.Void msg
  Flat _ g -> Tree.Flat <$> compileRule st g
  Call loc f args -> case Map.lookup f st.arities of
    Just ar
      | length args == ar -> Tree.Call f <$> compileRule st `mapM` args
      | otherwise -> Left [WrongNumberOfArguments loc ar f (length args)]
    Nothing -> Left [UndefinedRule loc f]
  Ctor _ name g -> Tree.Ctor name <$> compileRule st g

compileRep :: Tree.Rule -> Tree.Rule -> Int -> Maybe Int -> Tree.Rule
compileRep !g1 !g2 !n0 Nothing = loop n0
  where
  loop 0 = Tree.Star2 g1 g2
  loop !n = Tree.Seq [ g1, g2, loop (n - 1) ]
compileRep !g1 !g2 !n0 (Just !m0) = loop n0 m0
  where
  loop 0 0 = Tree.Empty
  loop 0 1 = Tree.Seq [ g1, g2 ] `Tree.Alt2` Tree.Empty
  loop 0 !m = (Tree.Seq [ g1, g2, loop 0 (m - 1) ]) `Tree.Alt2` Tree.Empty
  loop !n !m = Tree.Seq [ g1, g2, loop (n - 1) (m - 1) ]

compileSatisfy :: Map String CharSet -> SatClass -> Either [Error] CharSet
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

compileClasses :: [ClassDef] -> Either [Error] (Map String CharSet)
compileClasses clss = do
  () <- checkDuplicateNames getName DuplicateClassDefinitions clss
  let go defd = \case
        CyclicSCC circClss -> Left [CircularClassDefinitions (toError <$> circClss)]
        AcyclicSCC cls -> Map.union defd <$> compileClass defd cls
  foldM go Map.empty (stronglyConnComp $ toVertex <$> clss)
  where
  getName (_, it, _) = it
  toVertex :: ClassDef -> (ClassDef, String, [String])
  toVertex def@(_, (_, name), body) = (def, name, fv body)
  toError :: ClassDef -> (FwdRange, String)
  toError (_, nameLoc, _) = nameLoc
  fv :: CharClass -> [String]
  fv (ClassVar _ x) = [x]
  fv (ClassRange _ _ _) = []
  fv (ClassChar _ _) = []
  fv (ClassSet _ _) = []
  fv (ClassUnion a b) = fv a <> fv b
  fv (ClassMinus a b) = fv a <> fv b

compileClass :: Map String CharSet -> ClassDef -> Either [Error] (Map String CharSet)
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
     (a -> (FwdRange, String)) -- ^ map a definition to its (located) name
  -> ([FwdRange] -> String -> Error) -- ^ map a name and its multiple definition locations to an error
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
