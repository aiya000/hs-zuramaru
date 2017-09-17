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

import Control.Exception.Safe (Exception, SomeException, toException)
import Control.Exception.Throwable.TH (declareException)
import Control.Monad.Fail (fail)
import Data.Extensible (castEff)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Maru.Type
import Prelude hiding (fail)
import TextShow (showt)
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified Maru.Eval.RuntimeOperation as OP

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens ((^?), _Just)
-- >>> import qualified Maru.Eval.RuntimeOperation as OP
-- >>> import qualified Data.Map.Lazy as M
-- >>> :{
-- >>> let modifiedEnv = M.insert "*y*" (AtomSymbol "*x*")
--                     $ M.insert "*x*" (AtomInt 10) initialEnv
-- >>> :}

declareException "EvalException" ["EvalException"]


-- |
-- An initial value of the runtime.
-- This is the empty.
initialEnv :: MaruEnv
initialEnv = M.fromList []


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


-- |
-- A naked evaluator of zuramaru
--
-- Evaluate a macro,
-- or Calculate a function
execute :: SExpr -> MaruEvaluator SExpr
-- def! and let* is the axioms
execute (Cons (AtomSymbol "def!") s) = defBang s
execute (Cons (AtomSymbol "let*") s) = letStar s
execute sexpr                        = call sexpr


-- |
-- def!
--
-- (def! *x* 10)
--
-- >>> (result, envWithX, _) <- flip runMaruEvaluator initialEnv $ defBang (Cons (AtomSymbol "*x*") (Cons (AtomInt 10) Nil))
-- >>> result
-- Right (AtomInt 10)
-- >>> M.lookup "*x*" envWithX ^? _Just 
-- Just (AtomInt 10)
--
-- Define "*y*" over "*x*"
-- (def! *y* *x*)
--
-- >>> (result, env, _) <- flip runMaruEvaluator envWithX $ defBang (Cons (AtomSymbol "*y*") (Cons (AtomSymbol "*x*") Nil))
-- >>> result
-- Right (AtomInt 10)
-- >>> M.lookup "*y*" env ^? _Just
-- Just (AtomInt 10)
--
-- Define "*z*" over a calculation (+ 1 2)
--
-- >>> (result, env, _) <- flip runMaruEvaluator initialEnv $ defBang (Cons (AtomSymbol "*z*") (Cons (Cons (AtomSymbol "+") (Cons (AtomInt 1) (Cons (AtomInt 2) Nil))) Nil))
-- >>> result
-- Right (AtomInt 3)
-- >>> M.lookup "*z*" env ^? _Just
-- Just (AtomInt 3)
defBang :: SExpr -> MaruEvaluator SExpr
defBang (Cons (AtomSymbol sym) s) =
  case s of
    -- e.g. (def! x (+ 1 2)) should set x to 3.
    -- `execute (def! x (+ 1 2))` (a fake notation)
    --    maps '(Cons (Cons + (Cons 1 (Cons 2 Nil))) Nil)'
    --    to 's'.
    --    ('x' is mapped to '(Cons + (Cons 1 (Cons 2 Nil)))')
    Cons x Nil -> defineSymToItsResult sym x
    _          -> defineSymToItsResult sym s
  where
    -- Calculate `SExpr`,
    -- and Set `MaruSymbol`
    defineSymToItsResult :: MaruSymbol -> SExpr -> MaruEvaluator SExpr
    defineSymToItsResult sym sexpr = do
      sexpr' <- execute sexpr
      modifyMaruEnv $ M.insert sym sexpr'
      return sexpr'
defBang s = throwFail $ "def!: an invalid condition is detected `" <> showt s <> "`"


-- |
-- let*
--
-- (let* (x 10) x)
--
-- >>> (result, env, _) <- flip runMaruEvaluator initialEnv $ letStar (Cons (Cons (AtomSymbol "x") (Cons (AtomInt 10) Nil)) (Cons (AtomSymbol "x") Nil))
-- >>> result
-- Right (AtomInt 10)
-- >>> M.lookup "x" env
-- Nothing
letStar :: SExpr -> MaruEvaluator SExpr
letStar (Cons (Cons (AtomSymbol sym) (Cons x Nil)) body) = do
  --TODO: Create new scope
  modifyMaruEnv $ M.insert sym x
  execute body
letStar s = fail $ "let*: an invalid condition is detected `" ++ show s ++ "`"


-- |
-- Call general function/macro (other than def!/let*).
-- If it has a head element, apply tail elements to the head element. (+ 1 2)
--
-- If it is a symbol, find the symbol from current environment (`MaruEnv`)
-- (Throw exception if it couldn't be found).
--
-- If it is an another primitive value, return just it.
--
-- (+ 1 2)
--
-- >>> (result, _, _) <- flip runMaruEvaluator initialEnv $ call (Cons (AtomSymbol "+") (Cons (AtomInt 1) (Cons (AtomInt 2) Nil)))
-- >>> let expected = runMaruCalculator $ OP.add [AtomInt 1, AtomInt 2]
-- >>> result == expected
-- True
--
-- *x*
--
-- >>> (result, _, _) <- flip runMaruEvaluator modifiedEnv $ call (AtomSymbol "*x*")
-- >>> result
-- Right (AtomInt 10)
--
-- *y*
--
-- >>> (result', _, _) <- flip runMaruEvaluator modifiedEnv $ call (AtomSymbol "*y*")
-- >>> result' == result
-- True
call :: SExpr -> MaruEvaluator SExpr
-- Extract a value
call (Cons x Nil) = call x

call (Cons (AtomSymbol sym) y) = do
  args <- mapM execute $ flatten y
  let maybeCalculator = realBody sym -- 「そうか、リアルボディ！！」
  case maybeCalculator of
    Just calc -> castEff $ calc args
    Nothing   -> error "TODO: implement SExpr behabior of a function"
  where
    -- Get the read body of `MaruFunc` from the symbol
    realBody :: MaruSymbol -> Maybe MaruFunc
    realBody "+" = Just OP.add
    realBody "-" = Just OP.sub
    realBody "*" = Just OP.times
    realBody "/" = Just OP.div
    realBody _   = Nothing

call (AtomSymbol sym) =
  lookupSymbol sym >>= \case
    AtomSymbol s -> call $ AtomSymbol s
    x            -> return x

call s@(Cons x _) = throwFail $ "expected a symbol, but '" <> showt x <> "' from '" <> showt s <> "'"
call (AtomInt x)  = return $ AtomInt x
call Nil          = return Nil
