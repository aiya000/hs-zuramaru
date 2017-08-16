{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Maru.Type.Eval
  ( ExceptionCause
  , MaruEvaluator
  , includeFail
  , runMaruEvaluator
  , Discriminating (..)
  , MaruEnv
  , MaruFunc
  , MaruMacro
  , SomeMaruPrimitive (..)
  , MaruPrimitive (..)
  , lookupSymbol
  , liftBinaryFunc
  , _SomeMaruPrimitive
  , (^$)
  , Result (..)
  ) where

import Control.Eff (Eff, Member, (:>))
import Control.Eff.Exception (runExc, throwExc)
import Control.Eff.Lift (Lift, runLift)
import Control.Eff.State.Lazy (State, runState, get)
import Control.Eff.Writer.Lazy (runMonoidWriter)
import Control.Lens (Prism', prism', Getting)
import Control.Monad.Fail (MonadFail(..))
import Data.Bifunctor (first)
import Data.Map.Lazy (Map)
import Data.Monoid ((<>), First)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Typeable (Typeable, typeRep)
import Data.Void (Void)
import Maru.Type.Eff (ExceptionCause, Fail', liftMaybe', SimplificationSteps, WriterSimplifSteps)
import Maru.Type.Lens ((^$?))
import Maru.Type.SExpr
import Prelude hiding (fail)
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

-- | A total effect of @MaruEvaluator@
type Eval = Fail' :> State MaruEnv :> WriterSimplifSteps :> Lift IO :> Void

-- | A monad for evaluating a program
type MaruEvaluator = Eff Eval

instance MonadFail MaruEvaluator where
  fail = throwExc . T.pack

--TODO: Can Fail' context include WriterSimplifSteps context ? (I want to catch a canceled logs for debugging)
--NOTE: Why eff's runState's type sigunature is different with mtl runState ?
-- | Run an evaluation of @MaruEvaluator a@
runMaruEvaluator :: MaruEvaluator a -> MaruEnv -> IO (Either ExceptionCause a, MaruEnv, SimplificationSteps)
runMaruEvaluator m env = fmap (flatten . first swap . swap) . runLift . runMonoidWriter . runState env $ runExc m
  where
    flatten :: ((a, b), c) -> (a, b, c)
    flatten ((x, y), z) = (x, y, z)


-- |
-- Like `liftEitherM`.
--
-- Include `Maybe` as `Fail'`.
-- If it is Nothing, the whole of MaruEvaluator to be failed.
includeFail :: ExceptionCause -> MaruEvaluator (Maybe a) -> MaruEvaluator a
includeFail cause mm = do
  maybeIt <- mm
  liftMaybe' cause maybeIt

--NOTE: Can this is alternated by some lens's function ?
-- |
-- This is like `Prism`'s accessor,
-- but don't return result as `Maybe`.
--
-- Simular to (^$?) but Nothing is included as a failure of the whole of `MaruEvaluator`.
--
-- `Typeable` for the error message.
(^$) :: (Typeable s, Typeable a) => MaruEvaluator s -> Getting (First a) s a -> MaruEvaluator a
(m :: MaruEvaluator s) ^$ (acs :: Getting (First a) s a) = do
  let typeNameOfS = T.pack . show $ typeRep (Proxy :: Proxy s)
      typeNameOfA = T.pack . show $ typeRep (Proxy :: Proxy a)
      cause = "(^$): `" <> typeNameOfA <> "` couldn't be getten from `" <> typeNameOfS <> "`"
  includeFail cause $ m ^$? acs


-- |
-- A part of newtype for `Either`.
-- Avoid orphan instance's declaration.
-- Also this is a concrete type.
newtype Result a = Result
  { unResult :: Either ExceptionCause a
  } deriving (Show, Eq, Functor, Applicative, Monad)

instance MonadFail Result where
  fail = Result . Left . T.pack


-- |
-- A function of maru.
-- It keeps the purity, no effects.
--
-- Take `[SExpr]` as arguments, its length is checked by each function.
-- If it is not the expected length, `Nothing` maybe given.
type MaruFunc = [SExpr] -> Result SExpr

-- |
-- A macro of maru, This is simular to `MaruFunc`.
-- This is possibility to update the state of the environment.
type MaruMacro = [SExpr] -> MaruEvaluator SExpr

-- | An identifier for dicriminate a type of @SomeMaruPrimitive@
data Discriminating :: * -> * where
  DiscrInt  :: Discriminating Int
  DiscrText :: Discriminating Text
  -- | The identifier for a function
  DiscrFunc :: Discriminating MaruFunc
  -- | The identifier for a macro
  DiscrMacro :: Discriminating MaruMacro


-- | The state of the runtime
type MaruEnv = Map Symbol SomeMaruPrimitive

-- | A reversible monomorphic type for @MaruPrimitive@
data SomeMaruPrimitive = forall a. MaruPrimitive a => SomeMaruPrimitive (Discriminating a) a

-- | A `Prism` for `SomeMaruPrimitive`
_SomeMaruPrimitive :: MaruPrimitive a => Discriminating a -> Prism' SomeMaruPrimitive a
_SomeMaruPrimitive DiscrInt = prism' (SomeMaruPrimitive DiscrInt) $
  \case SomeMaruPrimitive DiscrInt x -> Just x
        _ -> Nothing
_SomeMaruPrimitive DiscrText = prism' (SomeMaruPrimitive DiscrText) $
  \case SomeMaruPrimitive DiscrText x -> Just x
        _ -> Nothing
_SomeMaruPrimitive DiscrFunc = prism' (SomeMaruPrimitive DiscrFunc) $
  \case SomeMaruPrimitive DiscrFunc f -> Just f
        _ -> Nothing
_SomeMaruPrimitive DiscrMacro = prism' (SomeMaruPrimitive DiscrMacro) $
  \case SomeMaruPrimitive DiscrMacro f -> Just f
        _ -> Nothing


instance Show SomeMaruPrimitive where
  show x = "SomeMaruPrimitive " ++ case x of
    SomeMaruPrimitive DiscrInt  a -> show a
    SomeMaruPrimitive DiscrText a -> T.unpack a
    SomeMaruPrimitive DiscrFunc  _ -> "#func"
    SomeMaruPrimitive DiscrMacro _ -> "#macro"


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

instance MaruPrimitive MaruFunc where
  fromSExpr (AtomSymbol x) = lookupSymbol x ^$ _SomeMaruPrimitive DiscrFunc
  fromSExpr _ = fail "it cannot be converted to MaruPrimitive (Int -> Int -> Int)"

instance MaruPrimitive MaruMacro where
  fromSExpr (AtomSymbol x) = lookupSymbol x ^$ _SomeMaruPrimitive DiscrMacro
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
