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

--TODO: Organize document comments

-- |
-- @MaruEvaluator@ evaluates @SEexpr@.
--
-- NOTE:
-- Please see below items for each quasi-quote in the doctest
--
-- - 'Maru.QQ.ShortName' ('Maru.QQ')
-- - 'Maru.Type.SExpr'
module Maru.Eval
  ( initialEnv
  , eval
  , letStar
  , call
  , do_
  , if_
  , binding
  , funcall
  , hiMaruEnv
  , print_
  , list
  ) where

import Control.Exception.Safe (Exception, SomeException, toException)
import Control.Exception.Throwable.TH (declareException)
import Control.Lens ((^?))
import Control.Monad (when)
import Control.Monad.Fail (fail)
import Data.Extensible (castEff)
import Data.Function ((&))
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Maru.Type
import Prelude hiding (fail)
import TextShow (showt)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Maru.Eval.RuntimeOperation as OP
import qualified Maru.Type.SExpr as MSym (pack)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
-- >>> :set -XQuasiQuotes
-- >>> import Control.Lens (_Just)
-- >>> import Maru.QQ.ShortName (z, pp)
-- >>> import qualified Maru.Eval.RuntimeOperation as OP
-- >>> import qualified Maru.Type.Eval as TE
-- >>> let runMaruEvaluatorDefault = flip runMaruEvaluator initialEnv
-- >>> :{
-- >>> let modifiedEnv = initialEnv <>
--                         [[ ("*x*", AtomInt 10)
--                          , ("*y*", AtomSymbol "*x*")
--                          ]]
-- >>> :}

declareException "EvalException" ["EvalException"]


--TODO: AtomSymbol is not the string literal ! Avoid this v
--      zuramaru> do
--      EvalException: "A symbol '#core-macro' is not found"
--      zuramaru> def!
--      EvalException: "A symbol '#core-macro' is not found"
--      zuramaru> let*
--      EvalException: "A symbol '#core-macro' is not found"
--      these should be v
--      zuramaru> do
--      #core-macro

-- |
-- An initial value of the runtime.
-- This is the empty.
initialEnv :: MaruEnv
initialEnv = [[ ("nil", Nil) --TODO: Should this mapping is resolved in the parser or the preprocessor ?
              , ("def!", AtomSymbol "#core-macro")
              , ("let*", AtomSymbol "#core-macro")
              , ("do", AtomSymbol "#core-macro")
              , ("if", AtomSymbol "#core-macro")
              , ("fn*", AtomSymbol "#core-macro")
              , ("print", AtomSymbol "#core-macro")
              , ("list", AtomSymbol "#core-macro")
              ]]


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
flatten (Quote x)      = [Quote x]


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
-- the forms of '((fn* xxx yyy) z)' are applied as a function
execute (Cons (Cons (AtomSymbol "fn*") (Cons params (Cons body Nil))) args) = execMacro funcall $ Cons params (Cons body (Cons args Nil))
-- the forms of '(fn* xxx yyy)' of '(let* (f (fn* xxx yyy)))' are evaluated
execute (Cons (AtomSymbol "fn*") s) = execMacro binding s
-- `hi-maru-env` for debug
execute (Cons (AtomSymbol "hi-maru-env") Nil) = execMacro hiMaruEnv Nil
execute (Cons (AtomSymbol "print") s) = execMacro print_ s
execute (Cons (AtomSymbol "list") s) = execMacro list s
execute sexpr = execMacro call sexpr


-- |
-- let*
--
-- (let* (x 10) x)
--
-- >>> [z|(let* (x 10) x)|]
-- AtomInt 10
--
-- >>> (result, env, _) <- runMaruEvaluatorDefault $ execMacro letStar (Cons (Cons (AtomSymbol "x") (Cons (AtomInt 10) Nil)) (Cons (AtomSymbol "x") Nil))
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
-- >>> (result, _, _) <- runMaruEvaluatorDefault $ execMacro call (Cons (AtomSymbol "+") (Cons (AtomInt 1) (Cons (AtomInt 2) Nil)))
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
--
-- the quote
--
-- >>> (of10, _, _) <- flip runMaruEvaluator modifiedEnv $ execMacro call (Quote (AtomInt 10))
-- >>> of10
-- Right (AtomInt 10)
--
-- >>> (ofSym, _, _) <- flip runMaruEvaluator modifiedEnv $ execMacro call (Quote (AtomSymbol "some---symbol"))
-- >>> ofSym
-- Right (AtomSymbol "some---symbol")
call :: MaruMacro
-- Extract a value
call = MaruMacro call'
  where
    call' :: SExpr -> MaruEvaluator SExpr
    call' (AtomInt x)    = return $ AtomInt x
    call' (AtomBool x)   = return $ AtomBool x
    call' Nil            = return Nil
    call' (Quote x)      = return x

    -- Look up the value from the current environment
    call' (AtomSymbol sym) =
      lookupVar sym >>= \case
        AtomSymbol s -> call' $ AtomSymbol s
        --TODO: Currently, sym is regarded to the string value. Because the string literal is not implemented at now. Don't regard to the string value, throw the exception with the cause of "the symbol is not found".
        x -> return x

    call' (Cons Nil Nil) = return Nil -- `()` is evaluted to `()`

    -- The axiomly functions
    call' (Cons (AtomSymbol "+") args) = mapM execute (flatten args) >>= castEff . execFunc OP.add
    call' (Cons (AtomSymbol "-") args) = mapM execute (flatten args) >>= castEff . execFunc OP.sub
    call' (Cons (AtomSymbol "*") args) = mapM execute (flatten args) >>= castEff . execFunc OP.times
    call' (Cons (AtomSymbol "/") args) = mapM execute (flatten args) >>= castEff . execFunc OP.div
    --
    call' (Cons (AtomSymbol sym) args) = do
      val <- lookupVar sym
      execute $ Cons val args

    call' s@(Cons x _) = throwFail $ "expected the symbol of the function or the macro, but got `" <> showt x <> "` (in `" <> showt s <> "`)"


-- |
-- def!
--
-- (def! *x* 10)
--
-- >>> (result, envWithX, _) <- runMaruEvaluatorDefault $ execMacro defBang (Cons (AtomSymbol "*x*") (Cons (AtomInt 10) Nil))
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
-- >>> (result, env, _) <- runMaruEvaluatorDefault $ execMacro defBang (Cons (AtomSymbol "*z*") (Cons (Cons (AtomSymbol "+") (Cons (AtomInt 1) (Cons (AtomInt 2) Nil))) Nil))
-- >>> result
-- Right (AtomInt 3)
-- >>> TE.lookup "*z*" env ^? _Just
-- Just (AtomInt 3)
defBang :: MaruMacro
defBang = MaruMacro $ \case
  Cons (AtomSymbol sym) (Cons val Nil) -> do
    val' <- execute val
    insertGlobalVar sym val'
    return val'
  s -> returnInvalid "def!" s


-- |
-- 'do' macro evaluates all the taken arguments sequentially
--
-- >>> :{
-- >>> [z|
-- >>>  (do
-- >>>    (def! x 10)
-- >>>    (def! y (+ x 1))
-- >>>    (def! z (+ y 1)))
-- >>> |] == [pp|12|]
-- >>> :}
-- True
--
-- >>> [z|(do)|] == [pp|()|] -- () is nil
-- True
do_ :: MaruMacro
do_ = MaruMacro $ \case
  Nil -> return Nil
  -- Don't evaluate `(x)` to `x`
  s@(Cons _ Nil) -> returnInvalid "do" s
  sexpr -> do
    let evaluatees = flatten sexpr
    xs <- mapM execute evaluatees
    if null xs
      then throwFail "do: fatal error !!" -- This case is already avoided by the above `Nil` pattern
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
-- Bind variables to constant values on `fn*` block (= a function) is evaluated,
-- but parameters (dummy arguments) and +,-,*,/ (basic functions) are kept.
--
-- A pair of this and 'funcall' is `fn*` macro.
--
-- >>> :{
-- >>> [z|
-- >>>  (do
-- >>>    (def! z 10)
-- >>>    (def! y (- 1 z))
-- >>>    (def! x (+ y z))
-- >>>    (fn* (a) (+ (- 1 1) 1)))
-- >>> |] == [pp|(fn* (a) (+ (- 1 1) 1))|]
-- >>> :}
-- True
--
-- Keep lexcal scopes (variable overlappnig)
--
-- >>> :{
-- >>> let result = [z|
-- >>>   (do
-- >>>     (def! x 0)
-- >>>     (print x)
-- >>>     (fn* (x) x))
-- >>> |]
-- >>> case result of
-- >>>      [pp|(fn* (x) 0)|] -> "bad"
-- >>>      [pp|(fn* (x) x)|] -> "good"
-- >>> :}
-- "good"
binding :: MaruMacro
binding = MaruMacro $ \case
  Cons params body -> do
    let cause = "fn* (caller): the function's formal parameter must be the symbol, but another things are specified: `" <> showt params <> "`"
    params' <- includeFail cause . return $ flatten params ^? asSymbolList
    expandedBody <- expandVarsWihtoutParams params' body
    return $ Cons (AtomSymbol "fn*") (Cons params expandedBody)
  s -> returnInvalid "fn* (caller)" s
  where
    -- Similar to 'expandVars',
    -- but if the 'MaruSymbol' is included in taken ['MaruSymbol'],
    -- it is not expanded
    expandVarsWihtoutParams :: [MaruSymbol] -> SExpr -> MaruEvaluator SExpr
    expandVarsWihtoutParams _ (AtomSymbol "+") = return $ AtomSymbol "+"
    expandVarsWihtoutParams _ (AtomSymbol "-") = return $ AtomSymbol "-"
    expandVarsWihtoutParams _ (AtomSymbol "*") = return $ AtomSymbol "*"
    expandVarsWihtoutParams _ (AtomSymbol "/") = return $ AtomSymbol "/"
    expandVarsWihtoutParams _ Nil = return Nil
    expandVarsWihtoutParams _ (AtomInt x) = return $ AtomInt x
    expandVarsWihtoutParams _ (AtomBool x) = return $ AtomBool x
    expandVarsWihtoutParams _ (Quote x) = return $ Quote x
    expandVarsWihtoutParams params' (Cons x y) = Cons <$> expandVarsWihtoutParams params' x <*> expandVarsWihtoutParams params' y
    expandVarsWihtoutParams params' (AtomSymbol var) =
      if var `elem` params' then return $ AtomSymbol var
                            else lookupVar var >>= expandVarsWihtoutParams params'


-- |
-- Apply arguments (real arguments) to parameters (dummy arguments) of a function.
--
-- A pair of this and 'funcall' is `fn*` macro.
--
-- >>> :{
-- >>> [z|
-- >>>   ((fn* (x y)
-- >>>     (+ x y)
-- >>>    ) 1 2)
-- >>> |]
-- >>> :}
-- AtomInt 3
--
-- `this` macro is expanded to a binding myself with a unique symbol,
-- it can make recursive functions.
--
-- >>> :{
-- >>> [z|
-- >>>   (let* f (fn* (x)
-- >>>             (if (<= 0 x)
-- >>>                 0
-- >>>                 (+ x (this (- x 1)))))
-- >>>     (f 5))
-- >>> |]
-- >>> :}
-- AtomInt 15
--
-- Always `this` means a mostly inner if `fn*` nests
--
-- >>> :{
-- >>> [z|
-- >>>   ((fn* (a)
-- >>>       ((fn* (x)
-- >>>           (if (<= 0 x)
-- >>>               0
-- >>>               (this (- x 1))))
-- >>>         (- a 1)))
-- >>>     5)
-- >>> |]
-- >>> :}
-- AtomInt 10
funcall :: MaruMacro
funcall = MaruMacro $ \s -> case flatten s of
  [params, body, args] -> do
    let cause = "fn* (callee): the function's formal parameter must be the symbol, but another things are specified: `" <> showt params <> "`"
    mappee <- includeFail cause . return $ flatten params ^? asSymbolList
    mapper <- mapM execute $ flatten args
    when (length mappee /= length mapper) .
      throwFail $ "fn* (callee): the dummy params and the real args are different length: params `" <> showt mappee <> "`, args `" <> showt mapper <> "`"
    let mapping = map (uncurry substituteVar) $ zip mappee mapper
    execute $ foldl' (&) body mapping
  _  -> returnInvalid "fn* (callee)" s


-- |
-- For debug.
-- Take out 'MaruEnv' of 'MaruScopes' as the list of cons.
hiMaruEnv :: MaruMacro
hiMaruEnv = MaruMacro $ \_ ->
  --TODO: AtomSymbol is not the string literal !! Implement string literal
  AtomSymbol . MSym.pack . show <$> getMaruEnv


-- | Print nothing or a S expression on the screen
print_ :: MaruMacro
print_ = MaruMacro $ nilOf . \case
  Nil -> liftIOEff $ putStr ""
  sexpr@(Cons _ _) -> do
    sexprs <- mapM ((readable <$>) . execute) $ flatten sexpr
    liftIOEff $ case nonEmpty sexprs of
                     Nothing      -> putStr ""
                     Just (x:|xs) -> T.putStr $ foldl' (<<>>) x xs
  x -> returnInvalid "print" x
  where
    nilOf :: MaruEvaluator a -> MaruEvaluator SExpr
    nilOf x = x >> return Nil

    (<<>>) :: Text -> Text -> Text
    x <<>> y = x <> "\n" <> y


-- |
-- Make a list with arguments
--
-- >>> [z|(list)|] == [pp|()|]
-- True
-- >>> [z|(list 1 2 3)|] == [pp|(1 2 3)|]
-- True
--
-- each arguments are evaluated
--
-- >>> [z|(list (+ 1 2) (+ 3 4))|] == [pp|(3 7)|]
-- True
list :: MaruMacro
list = MaruMacro $ fmap scottEncode . mapM execute . flatten
