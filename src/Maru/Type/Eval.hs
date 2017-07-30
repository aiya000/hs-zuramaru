{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Maru.Type.Eval
  ( ExceptionCause
  , MaruEvaluator
  , includeFail
  , runMaruEvaluator
  , Discriminating (..)
  , MaruEnv
  , MaruMacro
  , SomeMaruPrimitive (..)
  , MaruPrimitive (..)
  , lookupSymbol
  , liftBinaryFunc
  , _SomeMaruPrimitive
  ) where

import Control.Eff (Eff, Member, (:>))
import Control.Eff.Exception (runExc, throwExc)
import Control.Eff.Lift (Lift, runLift)
import Control.Eff.State.Lazy (State, runState, get)
import Control.Eff.Writer.Lazy (runMonoidWriter)
import Control.Lens (Getting, makeLenses)
import Control.Monad.Fail (MonadFail(..))
import Data.Bifunctor (first)
import Data.Map.Lazy (Map)
import Data.Monoid ((<>), First)
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Void (Void)
import Maru.Type.Eff (ExceptionCause, Fail', liftMaybe', SimplificationSteps, WriterSimplifSteps)
import Maru.Type.SExpr
import Maru.Type.SExpr (SExpr(..), SExprLike(..))
import Prelude hiding (fail)
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

-- | A total effect of @MaruEvaluator@
type Eval = Fail' :> State MaruEnv :> WriterSimplifSteps :> Lift IO :> Void

-- | A monad for evaluating a program
type MaruEvaluator = Eff Eval

instance MonadFail MaruEvaluator where
  fail = throwExc . T.pack

-- |
-- Like `liftEitherM`.
--
-- Include `Maybe` as `Fail'`.
-- If it is Nothing, the whole of MaruEvaluator to be failed.
includeFail :: ExceptionCause -> MaruEvaluator (Maybe a) -> MaruEvaluator a
includeFail cause mm = do
  maybeIt <- mm
  liftMaybe' cause maybeIt

--TODO: Can Fail' context include WriterSimplifSteps context ? (I want to catch a canceled logs for debugging)
--NOTE: Why eff's runState's type sigunature is different with mtl runState ?
-- | Run an evaluation of @MaruEvaluator a@
runMaruEvaluator :: MaruEvaluator a -> MaruEnv -> IO (Either ExceptionCause a, MaruEnv, SimplificationSteps)
runMaruEvaluator m env = fmap (flatten . first swap . swap) . runLift . runMonoidWriter . runState env $ runExc m
  where
    flatten :: ((a, b), c) -> (a, b, c)
    flatten ((x, y), z) = (x, y, z)


-- | A maru's macro, it has a side of a function of Haskell
type MaruMacro = Symbol -> SExpr -> MaruEvaluator SExpr

-- | A modifier for dicriminate a type of @SomeMaruPrimitive@
data Discriminating :: * -> * where
  DiscrInt          :: Discriminating Int
  DiscrText         :: Discriminating Text
  --TODO: Rename to `DiscrIntToIntToInt`
  DiscrIntXIntToInt :: Discriminating (Int -> Int -> Int)
  -- | For macros. Update @MaruEnv@.
  DiscrMacro        :: Discriminating MaruMacro


-- | The state of the runtime
type MaruEnv = Map Symbol SomeMaruPrimitive

-- | A reversible monomorphic type for @MaruPrimitive@
data SomeMaruPrimitive = forall a. MaruPrimitive a => SomeMaruPrimitive (Discriminating a) a

-- | A `Getting` for `SomeMaruPrimitive` (A `Prism`)
_SomeMaruPrimitive :: MaruPrimitive a => Discriminating a -> Getting (First a) SomeMaruPrimitive a
_SomeMaruPrimitive DiscrInt          = undefined
_SomeMaruPrimitive DiscrText         = undefined
_SomeMaruPrimitive DiscrIntXIntToInt = undefined
_SomeMaruPrimitive DiscrMacro        = undefined


instance Show SomeMaruPrimitive where
  show x = "SomeMaruPrimitive " ++ case x of
    SomeMaruPrimitive DiscrInt  a -> show a
    SomeMaruPrimitive DiscrText a -> T.unpack a
    SomeMaruPrimitive DiscrIntXIntToInt _ -> "#(Int   -> Int -> Int)"
    SomeMaruPrimitive DiscrMacro _        -> "#macro"


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
  fromSExpr :: SExpr -> MaruEvaluator a

instance MaruPrimitive Int where
  fromSExpr (AtomInt x) = return x
  fromSExpr _ = fail "it cannot be converted to MaruPrimitive Int"

-- | As a symbol
instance MaruPrimitive Text where
  fromSExpr (AtomSymbol (Symbol x)) = return x
  fromSExpr _ = fail "it cannot be converted to MaruPrimitive Text"

instance MaruPrimitive (Int -> Int -> Int) where
  fromSExpr (AtomSymbol x) = do
    SomeMaruPrimitive DiscrIntXIntToInt f <- lookupSymbol x
    return f
  fromSExpr _ = fail "it cannot be converted to MaruPrimitive (Int -> Int -> Int)"

instance MaruPrimitive MaruMacro where
  fromSExpr (AtomSymbol x) = do
    SomeMaruPrimitive DiscrMacro f <- lookupSymbol x
    return f
  fromSExpr _ = fail "it cannot be converted to MaruPrimitive MaruMacro"


-- |
-- Take a value from @MaruEnv@ in @State@.
-- If @sym@ is not exists, take invalid value of @Exc NoSuchSymbolException'@
lookupSymbol :: forall r. (Member Fail' r, Member (State MaruEnv) r)
             => Symbol -> Eff r SomeMaruPrimitive
lookupSymbol sym = do
  env <- get
  liftMaybe' ("A symbol '" <> unSymbol sym <> "' is not found") $ M.lookup sym (env :: MaruEnv)


-- |
-- Lift a binary function of @MaruTerm@ to a binary function of @SExpr@.
-- Take a valid value if both types of @x@ and @y@ are a value of @MaruTerm@ (e.g. symbol, @Int@).
-- Take a invalid value otherwise.
liftBinaryFunc :: (MaruPrimitive a, SExprLike a) => (a -> a -> a) -> SExpr -> SExpr -> MaruEvaluator SExpr
liftBinaryFunc f x y = do
  x' <- fromSExpr x
  y' <- fromSExpr y
  return . wrap $ f x' y'
