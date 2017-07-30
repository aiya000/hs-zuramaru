{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | @MaruEvaluator@ evaluates @SEexpr@.
module Maru.Eval
  ( initialEnv
  , eval
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.Exception (throwExc)
import Control.Exception.Safe (Exception, SomeException, toException)
import Control.Exception.Throwable.TH (declareException)
import Control.Monad (foldM)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Maru.Type (SExpr(..), nonEmpty', Fail', SimplificationSteps, Symbol(..), includeFail, _SomeMaruPrimitive, (^$?))
import Maru.Type.Eval
import qualified Control.Eff.State.Lazy as STL
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

declareException "EvalException" ["EvalException"]


-- |
-- An initial value of the runtime of evaluation.
--
-- This is a state of @MaruEvaluator@.
--
-- This maybe passed to @eval@
initialEnv :: MaruEnv
initialEnv = M.fromList [ ("+", SomeMaruPrimitive DiscrIntXIntToInt (+))
                        , ("-", SomeMaruPrimitive DiscrIntXIntToInt (-))
                        , ("*", SomeMaruPrimitive DiscrIntXIntToInt (*))
                        , ("/", SomeMaruPrimitive DiscrIntXIntToInt div)
                        , ("set", SomeMaruPrimitive DiscrMacro set)
                        , ("find", SomeMaruPrimitive DiscrMacro find)
                        , ("get", SomeMaruPrimitive DiscrMacro get)
                        ]
  where
    set :: MaruMacro
    set sym (AtomInt x) = do
      env <- STL.get
      STL.put $ M.insert sym (SomeMaruPrimitive DiscrInt x) env
      return $ AtomSymbol sym
    --TODO: Add patterns if primitives of other than AtomInt is added (MaruPrimitive may help this)
    set _ _ = error "TODO (Maru.Eval.set)"

    find :: MaruMacro
    find = undefined

    get :: MaruMacro
    get = undefined


-- |
-- Evaluate a S expression,
-- and happen its side effects.
--
-- If you don't have a value of @MaruEnv@, you can use @initialEnv@.
--
-- Return an evaluated result, with new @MaruEnv@
-- (@env@ is changed if the evaluation of @SExpr@ changes @MaruEnv@).
eval :: MaruEnv -> SExpr -> IO (Either SomeException (SExpr, MaruEnv, SimplificationSteps))
eval env sexpr = do
  (result, newEnv, simplifLogs) <- runMaruEvaluator (execute sexpr) env
  case result of
    Left cause  -> return . Left . toException $ EvalException (T.unpack cause) sexpr
    Right sexpr -> return $ Right (sexpr, newEnv, simplifLogs)


--NOTE: This logic maybe not enough (e.g. (set x (set y 10)) should be evaluated to (AtomSymbol "x"))
-- | A naked evaluator of zuramaru
execute :: SExpr -> MaruEvaluator SExpr

-- Evaluate a macro
execute (Cons (AtomSymbol sym) (Cons x xs)) = do
  SomeMaruPrimitive DiscrMacro g <- lookupSymbol sym
  g sym x

-- Evaluate a function
execute (Cons (AtomSymbol x) xs) = do
  let cause = unSymbol x <> "'s entity should be (Int -> Int -> Int)"
  f <- includeFail cause $ lookupSymbol' x ^$? _SomeMaruPrimitive DiscrIntXIntToInt
  -- Evaluate recursively
  xs' <- flatten xs >>= mapM execute >>= nonEmpty'
  foldM1 (liftBinaryFunc f) xs'
    where
      lookupSymbol' :: Symbol -> MaruEvaluator SomeMaruPrimitive
      lookupSymbol' = lookupSymbol

execute (Cons (AtomInt x) Nil) = return $ AtomInt x
execute (Cons x y)             = return $ Cons x y
execute (AtomInt x)            = return $ AtomInt x
execute Nil                    = return Nil
execute (Quote _)              = error "TODO (eval)"

execute (AtomSymbol (Symbol x)) = throwExc ("An operator (" <> x <> ") is specified without any argument" :: ExceptionCause)


-- |
-- Extact a first layer.
-- Also don't touch a second layer and more.
--
-- >>> let x = Cons (AtomInt 1) (Cons (AtomInt 2) (Cons (AtomInt 3) Nil)) -- (1 2 3)
-- >>> flatten x
-- [AtomInt 1, AtomInt 2, AtomInt 3]
-- >>> let y = Cons (AtomInt 2) (Cons (AtomSymbol "*") (Cons (AtomInt 3) (Cons (AtomInt 4) Nil))) -- (2 (* 3 4))
-- >>> flatten y
-- [AtomInt 2, Cons (AtomSymbol "*") (Cons (AtomInt 3) (Cons (AtomInt 4) Nil))]
-- >> let z = Cons (AtomSymbol "*") (Cons (AtomInt 3) (Cons (AtomInt 4) Nil)) -- (* 3 4)
-- >> flatten z
-- [Cons (AtomSymbol "*") (Cons (AtomInt 3) (Cons (AtomInt 4) Nil))]
--
-- >>> let a = Cons (AtomSymbol "+") (Cons (AtomSymbol "*") (Cons (AtomSymbol "+") Nil)) -- (+ (* +))
flatten :: Member Fail' r => SExpr -> Eff r [SExpr]
flatten (Cons (AtomInt x) y) = (:) <$> pure (AtomInt x) <*> flatten y

flatten s@(Cons (AtomSymbol _) _) = return [s]
flatten s@(AtomInt _)             = return [s]
flatten s@(AtomSymbol _)          = return [s]
flatten Nil                       = return []

flatten (Cons _ _) = throwExc ("an unexpected case is detected (flatten)" :: ExceptionCause)
flatten (Quote _)  = error "TODO (flatten)"


-- | Simular to @foldM@ but for @NonEmpty@
foldM1 :: Monad m => (a -> a -> m a) -> NonEmpty a -> m a
foldM1 f (x :| xs) = foldM f x xs
