{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | @MaruEvaluator@ evaluates @SEexpr@.
module Maru.Eval
  ( MaruEnv
  , initialEnv
  , eval
  ) where

import Control.Exception.Safe (Exception)
import Control.Exception.Throwable.TH (declareException)
import Control.Monad (mzero)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Lazy (StateT, runStateT, get)
import Control.Monad.Trans.Either (EitherT(..), runEitherT)
import Data.Map.Lazy (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Maru.Type (SExpr(..), MaruTerm(..))
import qualified Data.Map.Lazy as M

-- Define a data type and instances
declareException "NoSuchSymbolException" ["NoSuchSymbolException"]

-- | This is thrown if looking up symbol is failed in @MaruEvaluator@
type NoSuchSymbolException' = NoSuchSymbolException ()

-- | For @(Monoid a, Monad m) => MonadPlus (EitherT a m)@
instance Monoid a => Monoid (NoSuchSymbolException a) where
  (NoSuchSymbolException cause clue) `mappend` (NoSuchSymbolException cause' clue')
    = NoSuchSymbolException (cause ++ " <> " ++ cause') (clue <> clue')
  mempty = NoSuchSymbolException "This is an empty NoSuchSymbolException" mempty

--NOTE: Shall I fix the orphan instance ?
instance (Monoid a, Monad m) => MonadFail (EitherT a m) where
  fail _ = mzero


-- | A monad for evaluating a program
newtype MaruEvaluator a = MaruEvaluator
  { _runMaruEvaluator :: EitherT NoSuchSymbolException' (StateT MaruEnv IO) a
  } deriving ( Functor, Applicative, Monad
             , MonadIO
             , MonadFail
             , MonadState MaruEnv
             )

-- | Run an evaluation of @MaruEvaluator@
runMaruEvaluator :: MaruEvaluator a -> MaruEnv -> IO (Either NoSuchSymbolException' a, MaruEnv)
runMaruEvaluator = runStateT . runEitherT . _runMaruEvaluator

-- | Run an evaluation of @MaruEvaluator@ and take only a result
evalMaruEvaluator :: MaruEvaluator a -> MaruEnv -> IO (Either NoSuchSymbolException' a)
evalMaruEvaluator = (fmap fst .) . runMaruEvaluator


type MaruEnv = Map Text SomeFunc

data SomeFunc = forall a b. SomeFunc (a -> b)


-- |
-- An initial value of the runtime of evaluation.
--
-- This is a state of @MaruEvaluator@.
--
-- This maybe passed to @eval@
initialEnv :: MaruEnv
initialEnv = M.fromList [ ("+", SomeFunc ((+) :: Int -> Int -> Int))
                        , ("-", SomeFunc ((-) :: Int -> Int -> Int))
                        , ("*", SomeFunc ((*) :: Int -> Int -> Int))
                        , ("/", SomeFunc (div :: Int -> Int -> Int))
                        ]

-- |
-- Evaluate a S expression,
-- and happen its side effects.
--
-- If you don't have a value of @MaruEnv@, you can use @initialEnv@.
--
-- Return an evaluated result, with new @MaruEnv@
-- (@env@ is changed if the evaluation of @SExpr@ changes @MaruEnv@).
eval :: MaruEnv -> SExpr -> IO (SExpr, MaruEnv)
eval env sexpr = do
  (result, newEnv) <- runMaruEvaluator (execute sexpr) env
  case result of
    Left  e -> print e >> return (Nil, newEnv)
    Right a -> return (a, newEnv)


-- |
-- A naked evaluator of zuramaru.
--
-- The error of pattern matching is delegated as @left@ value to MonadFail in here,
-- because @EitherT NoSuchSymbolException' IO@ is MonadFail.
execute :: SExpr -> MaruEvaluator SExpr
execute Nil       = return Nil
execute (Quote _) = error "TODO (eval)"
execute (Cons _ _) = undefined
execute (Atom (TermInt x)) = undefined --return x
execute (Atom (TermSymbol symbol)) = do
  env <- get
  let (Just val) = M.lookup symbol env
  undefined
