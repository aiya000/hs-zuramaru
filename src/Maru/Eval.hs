-- Suppress warnings what is happend by TemplateHaskell
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
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
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Maru.Type
import Prelude hiding (fail)
import TextShow (showt)
import qualified Data.Text as T
import qualified Maru.Eval.RuntimeOperation as OP

--TODO: Define an alias for `flip runMaruEvaluator initialEnv` to here, and use it in each doctest
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
-- >>> import Control.Lens ((^?), _Just)
-- >>> import qualified Maru.Eval.RuntimeOperation as OP
-- >>> import qualified Maru.Type.Eval as TE
-- >>> :{
-- >>> let modifiedEnv = initialEnv <>
--                         [[ ("*x*", AtomInt 10)
--                          , ("*y*", AtomSymbol "*x*")
--                          ]]
-- >>> :}

declareException "EvalException" ["EvalException"]


-- |
-- An initial value of the runtime.
-- This is the empty.
initialEnv :: MaruEnv
initialEnv = [[("nil", Nil)]]


-- |
-- Make the failure context with the message,
-- like "funcName: an invalid condition is detected `{invalidTerm}`"
-- ('return with the invalid term')
returnInvalid :: Text -> SExpr -> MaruEvaluator a
returnInvalid funcName invalidTerm = throwFail $ funcName <> ": an invalid condition is detected `" <> showt invalidTerm <> "`"


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
flatten (AtomBool x)   = [AtomBool x]
flatten (AtomSymbol x) = [AtomSymbol x]
flatten (Cons x y)     = [x] ++ flatten y


-- |
-- A naked evaluator of zuramaru
--
-- Evaluate a macro,
-- or Calculate a function
execute :: SExpr -> MaruEvaluator SExpr
-- def! and let* is the axioms
execute (Cons (AtomSymbol "def!") s) = execMacro defBang s
execute (Cons (AtomSymbol "let*") s) = execMacro letStar s
execute (Cons (AtomSymbol "do") s) = execMacro do_ s
execute (Cons (AtomSymbol "if") s) = execMacro if_ s
execute (Cons (AtomSymbol "fn*") s) = execMacro fnStar s
execute sexpr = execMacro call sexpr


-- |
-- let*
--
-- (let* (x 10) x)
--
-- >>> (result, env, _) <- flip runMaruEvaluator initialEnv $ execMacro letStar (Cons (Cons (AtomSymbol "x") (Cons (AtomInt 10) Nil)) (Cons (AtomSymbol "x") Nil))
-- >>> result
-- Right (AtomInt 10)
-- >>> TE.lookup "x" env
-- Nothing
letStar :: MaruMacro
letStar = MaruMacro $ \case
  Cons (Cons (AtomSymbol sym) (Cons x Nil)) (Cons body Nil) -> do
    newScope sym x
    result <- execute body
    popNewerScope
    return result
  s -> fail $ "let*: an invalid condition is detected `" ++ show s ++ "`"


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
-- >>> (result, _, _) <- flip runMaruEvaluator initialEnv $ execMacro call (Cons (AtomSymbol "+") (Cons (AtomInt 1) (Cons (AtomInt 2) Nil)))
-- >>> let expected = runMaruCalculator $ execFunc OP.add [AtomInt 1, AtomInt 2]
-- >>> result == expected
-- True
--
-- *x*
--
-- >>> (result, _, _) <- flip runMaruEvaluator modifiedEnv $ execMacro call (AtomSymbol "*x*")
-- >>> result
-- Right (AtomInt 10)
--
-- *y*
--
-- >>> (result', _, _) <- flip runMaruEvaluator modifiedEnv $ execMacro call (AtomSymbol "*y*")
-- >>> result' == result
-- True
call :: MaruMacro
-- Extract a value
call = MaruMacro call'
  where
    call' :: SExpr -> MaruEvaluator SExpr
    -- `()` is evaluted to `()`
    call' (Cons Nil Nil) = return Nil

    -- `(func {zero or more arguments})` is evaluated to its result.
    -- `func` is the symbol for the function.
    call' (Cons (AtomSymbol sym) y) = do
      args <- mapM execute $ flatten y
      let maybeCalculator = realBody sym -- 「そうか、リアルボディ！！」
      case maybeCalculator of
        Just calc -> castEff $ execFunc calc args
        Nothing   -> error "TODO: implement SExpr behabior of a function"

    -- `x` is looked up from the current environment (the `Eff`'s context with `MaruScopesAssociation`)
    call' (AtomSymbol sym) =
      lookupVar sym >>= \case
        AtomSymbol s -> call' $ AtomSymbol s
        --TODO: Currently, sym is regarded to the string value. Because the string literal is not implemented at now. Don't regard to the string value, throw the exception with the cause of "the symbol is not found".
        x -> return x

    call' s@(Cons x _) = throwFail $ "got '" <> showt x <> "' in '" <> showt s <> "', but expected the symbol of the function or the macro"
    call' (AtomInt x)  = return $ AtomInt x
    call' (AtomBool x) = return $ AtomBool x
    call' Nil          = return Nil

    -- Get the read body of `MaruFunc` from the symbol
    realBody :: MaruSymbol -> Maybe MaruFunc
    realBody "+" = Just OP.add
    realBody "-" = Just OP.sub
    realBody "*" = Just OP.times
    realBody "/" = Just OP.div
    realBody _   = Nothing


-- |
-- def!
--
-- (def! *x* 10)
--
-- >>> (result, envWithX, _) <- flip runMaruEvaluator initialEnv $ execMacro defBang (Cons (AtomSymbol "*x*") (Cons (AtomInt 10) Nil))
-- >>> result
-- Right (AtomInt 10)
-- >>> TE.lookup "*x*" envWithX ^? _Just 
-- Just (AtomInt 10)
--
-- Define "*y*" over "*x*"
-- (def! *y* *x*)
--
-- >>> (result, env, _) <- flip runMaruEvaluator envWithX $ execMacro defBang (Cons (AtomSymbol "*y*") (Cons (AtomSymbol "*x*") Nil))
-- >>> result
-- Right (AtomInt 10)
-- >>> TE.lookup "*y*" env ^? _Just
-- Just (AtomInt 10)
--
-- Define "*z*" over a calculation (+ 1 2)
--
-- >>> (result, env, _) <- flip runMaruEvaluator initialEnv $ execMacro defBang (Cons (AtomSymbol "*z*") (Cons (Cons (AtomSymbol "+") (Cons (AtomInt 1) (Cons (AtomInt 2) Nil))) Nil))
-- >>> result
-- Right (AtomInt 3)
-- >>> TE.lookup "*z*" env ^? _Just
-- Just (AtomInt 3)
defBang :: MaruMacro
defBang = MaruMacro $ \case
  Cons (AtomSymbol sym) s -> case s of
    Cons x Nil -> defineSymToItsResult sym x -- (def! x (+ 1 2)) should sets x to 3
    _          -> defineSymToItsResult sym s -- (def! x 10) should sets x to 10
  s -> returnInvalid "def!" s
  where
    -- Calculate `SExpr`,
    -- and Set `MaruSymbol`
    defineSymToItsResult :: MaruSymbol -> SExpr -> MaruEvaluator SExpr
    defineSymToItsResult sym sexpr = do
      sexpr' <- execute sexpr
      insertGlobalVar sym sexpr'
      return sexpr'


-- |
-- 'do' macro evaluates all the taken arguments sequentially.
--
-- `
-- (do
--  (def! x 10)
--  (def! y (+ x 1))
--  (def! z (+ y 1)))
-- `
-- returns 12.
--
-- >>> let sexpr = Cons (Cons (AtomSymbol "def!") (Cons (AtomSymbol "x") (Cons (AtomInt 10) Nil))) (Cons (Cons (AtomSymbol "def!") (Cons (AtomSymbol "y") (Cons (Cons (AtomSymbol "+") (Cons (AtomSymbol "x") (Cons (AtomInt 1) Nil))) Nil))) (Cons (Cons (AtomSymbol "def!") (Cons (AtomSymbol "z") (Cons (Cons (AtomSymbol "+") (Cons (AtomSymbol "y") (Cons (AtomInt 1) Nil))) Nil))) Nil))
-- >>> (result, _, _) <- flip runMaruEvaluator initialEnv $ execMacro do_ sexpr
-- >>> result
-- Right (AtomInt 12)
do_ :: MaruMacro
do_ = MaruMacro $ \case
  -- The calculation for `()` is not needed
  Cons Nil Nil -> return Nil
  -- Don't evaluate `(x)` to `x`
  s@(Cons _ Nil) -> returnInvalid "do" s
  sexpr -> do
    let evaluatees = flatten sexpr
    xs <- mapM execute evaluatees
    if null xs
      then throwFail "do: fatal error !!" -- This case is already avoided by the above `Cons Nil Nil` pattern
      else return $ last xs


-- |
-- 'if' macro branches to the arguments by the condition.
--
-- `(if true 10 20)` returns 10.
--
-- `(if false 10 20)` returns 20.
--
-- otherwise, the exception is thrown.
if_ :: MaruMacro
if_ = MaruMacro $ \case
  Cons cond (Cons x (Cons y Nil)) -> do
    cond' <- execute cond
    case cond' of
      AtomBool False -> execute y
      Nil            -> execute y
      _              -> execute x
  s -> returnInvalid "if" s


-- |
-- 'fn*' macro is a temporary function (it is often called 'lambda function').
--
-- Expand a S expression of its body (Make a 'expanded-fn*' expression).
--
-- If the conversion of S expression can be executed in any time (or if the completely immutability is promised),
-- this expansion can be the one of strategy in the real for the binding variables of the closure.
--
-- `
-- (def! x 10)
-- (fn* (a) x)
-- `
-- makes
-- `(expanded-fn* (a) 10)`
--
--
-- the expansion is recursively,
-- but only the symbol of +, -, *, / are not expanded.
-- `
-- (def! z 1)
-- (def! y (- 1 z)
-- (def! x (+ y z))
-- (fn* (a) x)
-- `
-- makes
-- `(expanded-fn* (a) (+ (- 1 1) 1))`
--
-- >>> :{
-- do
--    let modifiedEnv = initialEnv <>
--                            [[ ("z", AtomInt 1)
--                             , ("y", Cons (AtomSymbol "-") (Cons (AtomInt 1) (Cons (AtomSymbol "z") Nil)))
--                             , ("x", Cons (AtomSymbol "+") (Cons (AtomSymbol "y") (Cons (AtomSymbol "z") Nil)))
--                             ]]
--    let sexpr = Cons (Cons (AtomSymbol "a") Nil) (Cons (AtomSymbol "x") Nil)
--    (result, _, _) <- flip runMaruEvaluator modifiedEnv $ execMacro fnStar sexpr
--    return result
-- :}
-- Right (Cons (AtomSymbol "expanded-fn*") (Cons (Cons (AtomSymbol "a") Nil) (Cons (Cons (AtomSymbol "+") (Cons (Cons (AtomSymbol "-") (Cons (AtomInt 1) (Cons (AtomInt 1) Nil))) (Cons (AtomInt 1) Nil))) Nil)))
fnStar :: MaruMacro
fnStar = undefined
