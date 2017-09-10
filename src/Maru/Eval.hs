-- Suppress warnings what is happend by TemplateHaskell
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
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

import Control.Arrow ((>>>))
import Control.Exception.Safe (Exception, SomeException, toException)
import Control.Exception.Throwable.TH (declareException)
import Control.Lens (preview, (<&>))
import Data.Extensible (Associate, Eff, castEff)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Maru.Type
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified Maru.Eval.RuntimeOperation as OP

-- $setup
-- >>> :set -XOverloadedStrings

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
  loadMacro <- first' <$> (lookupSymbol sym <&> preview (_SomeMaruPrimitive DiscrMacro))
  loadFunc  <- first' <$> (lookupSymbol sym <&> preview (_SomeMaruPrimitive DiscrFunc))
  funcLike  <- liftFirst' $ loadMacro <> fmap (castEff .) loadFunc
  args      <- mapM execute $ flatten xs
  funcLike args
  where
    liftFirst' :: Associate FailKey FailValue xs
               => First' a -> Eff xs a
    liftFirst' = getFirst' >>> \case
      Left  e -> throwFail e
      Right a -> return a

execute (AtomInt x)    = return $ AtomInt x
execute (AtomSymbol x) = return $ AtomSymbol x
execute (Cons x y)     = Cons <$> execute x <*> execute y
execute Nil            = return Nil


-- |
-- >>> flatten Nil -- ()
-- []
-- >>> flatten $ AtomInt 10 -- 10
-- [AtomInt 10]
-- >>> flatten $ Cons (AtomInt 1) (Cons (AtomInt 2) (Cons (AtomInt 3) Nil)) -- (1 2 3)
-- [AtomInt 1,AtomInt 2,AtomInt 3]
-- >>> flatten $ Cons (AtomInt 2) (Cons (Cons (AtomInt 3) (Cons (AtomInt 4) (Cons (AtomInt 5) Nil))) Nil) -- (2 (3 4 5))
-- [AtomInt 2,Cons (AtomInt 3) (Cons (AtomInt 4) (Cons (AtomInt 5) Nil))]
-- >>> flatten $ Cons (Cons (AtomInt 1) (Cons (AtomInt 2) Nil)) (Cons (Cons (AtomInt 3) (Cons (AtomInt 4) Nil)) Nil) -- ((1 2) (3 4))
-- [Cons (AtomInt 1) (Cons (AtomInt 2) Nil),Cons (AtomInt 3) (Cons (AtomInt 4) Nil)]
flatten :: SExpr -> [SExpr]
flatten Nil            = []
flatten (AtomInt x)    = [AtomInt x]
flatten (AtomSymbol x) = [AtomSymbol x]
flatten (Cons x y)     = [x] ++ flatten y
