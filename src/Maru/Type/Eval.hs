{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Maru.Type.Eval
  ( ExceptionCause
  , Fail'
  , MaruEvaluator
  , runMaruEvaluator
  , Discriminating (..)
  , MaruEnv
  , SomeMaruPrimitive (..)
  , MaruPrimitive (..)
  , lookupSymbol
  , liftBinaryFunc
  , unsymbol
  ) where

import Control.Eff (Eff, Member, (:>))
import Control.Eff.Exception (runExc, throwExc)
import Control.Eff.Lift (Lift, runLift)
import Control.Eff.State.Lazy (State, runState, get)
import Control.Eff.Writer.Lazy (runMonoidWriter)
import Data.Bifunctor (first)
import Data.Map.Lazy (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Void (Void)
import Maru.Type.Eff (ExceptionCause, Fail', liftMaybe', SimplificationSteps, WriterSimplifSteps)
import Maru.Type.SExpr (SExpr(..), SExprLike(..))
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

-- | A total effect of @MaruEvaluator@
type Eval = Fail' :> State MaruEnv :> WriterSimplifSteps :> Lift IO :> Void

-- | A monad for evaluating a program
type MaruEvaluator a = Eff Eval a

--TODO: Can Fail' context include WriterSimplifSteps context ? (I want to catch a canceled logs for debugging)
--NOTE: Why eff's runState's type sigunature is different with mtl runState ?
-- | Run an evaluation of @MaruEvaluator a@
runMaruEvaluator :: MaruEvaluator a -> MaruEnv -> IO (Either ExceptionCause a, MaruEnv, SimplificationSteps)
runMaruEvaluator m env = fmap (flatten . first swap . swap) . runLift . runMonoidWriter . runState env $ runExc m
  where
    flatten :: ((a, b), c) -> (a, b, c)
    flatten ((x, y), z) = (x, y, z)


-- | A modifier for dicriminate a type of @SomeMaruPrimitive@
data Discriminating :: * -> * where
  DiscrInt          :: Discriminating Int
  DiscrText         :: Discriminating Text
  DiscrIntXIntToInt :: Discriminating (Int -> Int -> Int)

-- | The state of the runtime
type MaruEnv = Map Text SomeMaruPrimitive

-- | A reversible monomorphic type for @MaruPrimitive@
data SomeMaruPrimitive = forall a. MaruPrimitive a => SomeMaruPrimitive (Discriminating a) a

instance Show SomeMaruPrimitive where
  show x = "SomeMaruPrimitive " ++ case x of
    SomeMaruPrimitive DiscrInt  a -> show a
    SomeMaruPrimitive DiscrText a -> T.unpack a
    SomeMaruPrimitive DiscrIntXIntToInt _ -> "#(Int -> Int -> Int)"


-- |
-- A value of the runtime.
-- This has the partial commutual conversion with @SExpr@.
--
-- This is strongly associated with @MaruTerm@.
class MaruPrimitive a where
  -- |
  -- Get out @a@ from @SExpr@ if @SExpr@ represents @a@.
  --
  -- There is the possiblity to load a true (Haskell's) value of @a@ from @MaruEnv@.
  -- For example, the function may load its instance from @MaruEnv@ (also it maybe failed).
  fromSExpr :: (Member Fail' r, Member (State MaruEnv) r) => SExpr -> Eff r a

instance MaruPrimitive Int where
  fromSExpr (AtomInt x) = return x
  fromSExpr _ = fail "it cannot be converted to MaruPrimitive Int"

-- | As a symbol
instance MaruPrimitive Text where
  fromSExpr (AtomSymbol x) = return x
  fromSExpr _ = fail "it cannot be converted to MaruPrimitive Text"

instance MaruPrimitive (Int -> Int -> Int) where
  fromSExpr (AtomSymbol x) = do
    SomeMaruPrimitive DiscrIntXIntToInt f <- lookupSymbol x
    return f
  fromSExpr _ = fail "it cannot be converted to MaruPrimitive (Int -> Int -> Int)"


-- |
-- Take a value from @MaruEnv@ in @State@.
-- If @sym@ is not exists, take invalid value of @Exc NoSuchSymbolException'@
lookupSymbol :: forall r. (Member Fail' r, Member (State MaruEnv) r)
             => Text -> Eff r SomeMaruPrimitive
lookupSymbol sym = do
  env <- get
  liftMaybe' ("Symbol '" <> sym <> "' is not found") $ M.lookup sym (env :: MaruEnv)


-- |
-- Lift a binary function of @MaruTerm@ to a binary function of @SExpr@.
-- Take a valid value if both types of @x@ and @y@ are a value of @MaruTerm@ (e.g. symbol, @Int@).
-- Take a invalid value otherwise.
liftBinaryFunc :: (MaruPrimitive a, SExprLike a) => (a -> a -> a) -> SExpr -> SExpr -> MaruEvaluator SExpr
liftBinaryFunc f x y = do
  x' <- fromSExpr x
  y' <- fromSExpr y
  return . wrap $ f x' y'


--TODO: Can I use Prism instead ?
-- | Pull internal @Text@. If an argument is not @Atom (TermSymbol _)@, return an invalid value of @Fail'@
unsymbol :: Member Fail' r => SExpr -> Eff r Text
unsymbol (AtomSymbol x) = return x
unsymbol _ = throwExc ("An invalid value is taken, it is not the symbol" :: ExceptionCause)
