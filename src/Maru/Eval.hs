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
import Control.Lens ((&), _1, _2, (.~))
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
import qualified Maru.Type as MT

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens ((^?), _Just)
-- >>> import qualified Maru.Eval.RuntimeOperation as OP
-- >>> import qualified Data.Map.Lazy as M
-- >>> :{
-- >>> let modifiedEnv =
--       let x = SomeMaruPrimitive DiscrSExpr $ AtomInt 10
--           y = SomeMaruPrimitive DiscrSExpr $ AtomSymbol "*x*"
--       in M.insert "*y*" y $ M.insert "*x*" x initialEnv
-- >>> :}

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
-- >>> M.lookup "*x*" envWithX ^? _Just . _SomeMaruPrimitive DiscrSExpr
-- Just (AtomInt 10)
--
-- Define "*y*" over "*x*"
-- (def! *y* *x*)
--
-- >>> (result, env, _) <- flip runMaruEvaluator envWithX $ defBang (Cons (AtomSymbol "*y*") (Cons (AtomSymbol "*x*") Nil))
-- >>> result
-- Right (AtomInt 10)
-- >>> M.lookup "*y*" env ^? _Just . _SomeMaruPrimitive DiscrSExpr
-- Just (AtomInt 10)
--
-- Define "*z*" over a calculation (+ 1 2)
--
-- >>> let calc = (Cons (AtomSymbol "+") (Cons (AtomInt 1) (Cons (AtomInt 2) Nil)))
-- >>> (result, env, _) <- flip runMaruEvaluator initialEnv $ defBang (Cons (AtomSymbol "*z*") (Cons calc Nil))
-- >>> result
-- Right (AtomInt 3)
-- >>> M.lookup "*z*" env ^? _Just . _SomeMaruPrimitive DiscrSExpr
-- Just (AtomInt 3)
defBang :: SExpr -> MaruEvaluator SExpr
defBang s@(Cons (AtomSymbol _) (Cons x@(AtomSymbol _) _)) = do
  x' <- call x
  let s' = s & _Cons . _2 . _Cons . _1 .~ x'
  defBang s'
defBang (Cons (AtomSymbol sym) (Cons x _)) = do
  modifyMaruEnv . M.insert sym $ SomeMaruPrimitive DiscrSExpr x
  return x
defBang s = fail $ "def!: an invalid condition is detected `" ++ show s ++ "`"


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
  modifyMaruEnv . M.insert sym $ SomeMaruPrimitive DiscrSExpr x
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
call (Cons (AtomSymbol sym) y) = lookupSymbol sym >>= \case
  SomeMaruPrimitive DiscrFunc f -> do
    args <- mapM execute $ flatten y
    castEff $ f args
  SomeMaruPrimitive DiscrMacro f -> do
    args <- mapM execute $ flatten y
    f args
  SomeMaruPrimitive DiscrSExpr s ->
    return s

call (AtomSymbol sym) =
  lookupSymbol sym >>= \case
    --FIXME: Don't use the symbol as a text, create a term of text and use it instead
    f@(SomeMaruPrimitive DiscrFunc  _) -> return . AtomSymbol . MT.pack $ show f
    f@(SomeMaruPrimitive DiscrMacro _) -> return . AtomSymbol . MT.pack $ show f
    SomeMaruPrimitive DiscrSExpr s@(AtomSymbol _) -> call s
    SomeMaruPrimitive DiscrSExpr x -> return x

call s@(Cons x _) = throwFail $ "expected a symbol, but '" <> showt x <> "' from '" <> showt s <> "'"
call (AtomInt x)  = return $ AtomInt x
call Nil          = return Nil
