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
import Control.Eff.Exception (throwExc, liftEither)
import Control.Exception.Safe (Exception, SomeException, toException)
import Control.Exception.Throwable.TH (declareException)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Maru.Type (SExpr(..), Fail', SimplificationSteps, Symbol(..), _SomeMaruPrimitive, (^$?))
import Maru.Type.Eval
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified Maru.Eval.RuntimeOperation as OP

declareException "EvalException" ["EvalException"]


-- |
-- An initial value of the runtime of evaluation.
--
-- This is a state of @MaruEvaluator@.
--
-- This maybe passed to @eval@
initialEnv :: MaruEnv
initialEnv = M.fromList [ ("+", SomeMaruPrimitive DiscrFunc OP.add)
                        , ("-", SomeMaruPrimitive DiscrFunc OP.sub)
                        , ("*", SomeMaruPrimitive DiscrFunc OP.times)
                        , ("/", SomeMaruPrimitive DiscrFunc OP.div)
                        , ("set", SomeMaruPrimitive DiscrMacro OP.set)
                        , ("find", SomeMaruPrimitive DiscrMacro OP.find)
                        , ("get", SomeMaruPrimitive DiscrMacro OP.get)
                        ]


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


-- | A naked evaluator of zuramaru
execute :: SExpr -> MaruEvaluator SExpr

-- Evaluate a macro,
-- or Calculate a function
execute (Cons (AtomSymbol sym) xs) = do
  loadMacro <- first' <$> lookupSymbol sym ^$? _SomeMaruPrimitive DiscrMacro
  loadFunc  <- first' <$> lookupSymbol sym ^$? _SomeMaruPrimitive DiscrFunc
  funcLike  <- liftFirst' $ loadMacro <> fmap (upgradeEffects .) loadFunc
  args      <- flatten xs >>= mapM execute
  funcLike args
  where
    liftFirst' :: Member Fail' r => First' a -> Eff r a
    liftFirst' = liftEither . getFirst'

execute (Cons (AtomInt x) Nil)  = return $ AtomInt x
execute (Cons x y)              = return $ Cons x y
execute (AtomInt x)             = return $ AtomInt x
execute Nil                     = return Nil
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
flatten (Cons _ _)                = throwExc ("an unexpected case is detected (flatten)" :: ExceptionCause)
