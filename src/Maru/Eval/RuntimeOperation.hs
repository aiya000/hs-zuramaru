{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Define functions and macros, these are used in the runtime,
--
-- also these are through with `MaruEnv` (it should be `Maru.Eval.initialEnv`)
--
-- These respects clisp's behavior basically.
module Maru.Eval.RuntimeOperation
  ( add
  , sub
  , times
  , div
  , set
  , find
  , get
  ) where

import Control.Lens hiding (set)
import Control.Monad.Fail (fail)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Maru.Type
import Numeric.Extra (intToDouble)
import Prelude hiding (div, fail)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified Maru.Type as MT

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens hiding (set)
-- >>> import Data.Either
-- >>> import Maru.Eval
-- >>> import Maru.Type
-- >>> import qualified Data.Map.Lazy as M


ignoreAtomInt :: [SExpr] -> [SExpr]
ignoreAtomInt xs = xs ^.. folded . filtered (not . MT.isAtomInt)


-- |
-- Find all `AtomInt` elements,
-- Calculate its summation,
-- and wrap by `AtomInt`.
--
-- Return `AtomInt 0` if the given list is empty.
sumOfAtomInt :: [SExpr] -> SExpr
sumOfAtomInt = (AtomInt .) . sumOf $ folded . MT._AtomInt


-- |
-- >>> runMaruCalculator $ add [AtomInt 1, AtomInt 2]
-- Right (AtomInt 3)
-- >>> runMaruCalculator $ add []
-- Right (AtomInt 0)
-- >>> isLeft . runMaruCalculator $ add [AtomSymbol "xD"]
-- True
add :: MaruFunc
add xs = case ignoreAtomInt xs of
  [] -> return $ sumOfAtomInt xs
  invalidArgs -> fail $ "add: invalid arguments are given to (+): " ++ show invalidArgs


-- |
-- >>> runMaruCalculator $ sub [AtomInt 3, AtomInt 1]
-- Right (AtomInt 2)
-- >>> isLeft . runMaruCalculator $ sub []
-- True
-- >>> isLeft . runMaruCalculator $ sub [AtomSymbol "xD"]
-- True
sub :: MaruFunc
sub [] = fail "sub: takes a list of integer values, but took list is empty"
sub w@(x:xs) = case ignoreAtomInt w of
  [] -> return $ negativeSumOfAtomInt (x:|xs)
  invalidArgs -> fail $ "sub: invalid arguments are given to (-): " ++ show invalidArgs
  where
    -- head - tail
    negativeSumOfAtomInt :: NonEmpty SExpr -> SExpr
    negativeSumOfAtomInt (x:|xs) = sumOfAtomInt . (x:) . flip map xs $ MT.intBullet negate


-- |
-- >>> runMaruCalculator $ times [AtomInt 3, AtomInt 3]
-- Right (AtomInt 9)
-- >>> runMaruCalculator $ times []
-- Right (AtomInt 1)
-- >>> isLeft . runMaruCalculator $ times [AtomSymbol "xD"]
-- True
times :: MaruFunc
times xs = case ignoreAtomInt xs of
  [] -> return . AtomInt $ productOf (folded . MT._AtomInt) xs
  invalidArgs -> fail $ "times: invalid arguments are given to (*): " ++ show invalidArgs


--TODO: This makes an integral number unless like AtomRatio is implemented to SExpr
-- |
-- >>> runMaruCalculator $ div [AtomInt 3, AtomInt 3]
-- Right (AtomInt 1)
-- >>> isLeft . runMaruCalculator $ div []
-- True
-- >>> isLeft . runMaruCalculator $ div [AtomSymbol "xD"]
-- True
-- >>> isLeft . runMaruCalculator $ div [AtomInt 0, AtomInt 1]
-- True
-- >>> runMaruCalculator $ div [AtomInt 10, AtomInt 3]
-- Right (AtomInt 3)
-- >>> runMaruCalculator $ div [AtomInt 3, AtomInt 5]
-- Right (AtomInt 0)
div :: MaruFunc
div [] = fail "div: takes a non empty list, but took list is empty"
div w@(x:xs) = case (ignoreAtomInt w, negativeProductOfAtomInt (x:|xs)) of
  ([], Nothing) -> fail "div: 0 is divided by anything"
  ([], Just z)  -> return z
  (invalidArgs, _) -> fail $ "div: invalid arguments are given to (/): " ++ show invalidArgs
  where
    -- Safe (/)
    (/?) :: Maybe Double -> Double -> Maybe Double
    Nothing /? _ = Nothing
    Just 0  /? _ = Nothing
    Just x  /? y = Just $ x / y
    -- Extract [`Int`] from [`AtomInt`],
    -- and Convert each `Int` to `Double`
    doublesFromAtomInt :: NonEmpty SExpr -> [Double]
    doublesFromAtomInt = concatMap (maybeToList . (intToDouble <$>) . MT.unAtomInt) . NE.filter MT.isAtomInt
    -- All `NonEmpty SExpr` element are `AtomInt`.
    -- If 0 was devided, return `Nothing`.
    negativeProductOfAtomInt :: NonEmpty SExpr -> Maybe SExpr
    negativeProductOfAtomInt (doublesFromAtomInt -> (x:xs))
      = AtomInt . truncate <$> foldl' (/?) (Just x) xs
    negativeProductOfAtomInt _
      = Nothing


-- |
-- Set a variable of the name to a value,
-- and Return the just given name.
--
-- Take a first element of [`SExpr`] as a name.
-- Take a second element of [`SExpr`] as a value.
--
-- >>> (Right sexpr, newEnv, _) <- flip runMaruEvaluator initialEnv $ set [AtomSymbol "*x*", AtomInt 10]
-- >>> sexpr == AtomSymbol "*x*"
-- True
-- >>> M.lookup "*x*" newEnv ^? _Just . _SomeMaruPrimitive DiscrSExpr
-- Just (AtomInt 10)
-- >>> (Right sexpr, newEnv, _) <- flip runMaruEvaluator initialEnv $ set [AtomSymbol "this-is-an-undefined-variable", Nil]
-- >>> M.lookup "this-is-an-undefined-variable" newEnv ^? _Just . _SomeMaruPrimitive DiscrSExpr
-- Just Nil
set :: MaruMacro
set [] = fail "set: requires non empty arguments"
set (AtomSymbol sym:AtomInt x:_) = do
  modifyMaruEnv . M.insert sym $ SomeMaruPrimitive DiscrInt x
  return $ AtomSymbol sym
set xs = fail $ "set: an invalid condition is detected `" ++ show xs ++ "`"


-- |
-- Find a name from the current environment (`MaruEnv`).
--
-- Return the variable of the name if it is found.
-- Return `Nil` if it is not found.
--
-- >>> let modifiedEnv  = M.insert "*x*" (SomeMaruPrimitive DiscrSExpr $ AtomInt 10) initialEnv
-- >>> let modifiedEnv' = M.insert "*f*" (SomeMaruPrimitive DiscrSExpr $ AtomSymbol "set") modifiedEnv
-- >>> (Right sexpr, _, _) <- flip runMaruEvaluator modifiedEnv' $ find [AtomSymbol "*x*"]
-- >>> sexpr
-- AtomInt 10
-- >>> (Right sexpr, _, _) <- flip runMaruEvaluator modifiedEnv' $ find [AtomSymbol "*f*"]
-- >>> sexpr == AtomSymbol "set"
-- True
-- >>> (Right sexpr, _, _) <- flip runMaruEvaluator modifiedEnv' $ find [AtomSymbol "this-is-an-undefined-variable"]
-- >>> sexpr
-- Nil
find :: MaruMacro
find [] = fail "find: requires non empty arguments"
find (AtomSymbol sym:_) = do
  env <- getMaruEnv
  let maybeValue = M.lookup sym env >>= (^? _SomeMaruPrimitive DiscrInt)
  case maybeValue of
    Nothing -> return Nil
    Just x  -> return $ AtomInt x
find xs = fail $ "find: an invalid condition is detected `" ++ show xs ++ "`"


-- |
-- Similar to find,
-- but this `throwFail`s the exception if the given name is not found.
--
-- >>> let modifiedEnv = M.insert "*x*" (SomeMaruPrimitive DiscrSExpr $ AtomInt 10) initialEnv
-- >>> let modifiedEnv' = M.insert "*f*" (SomeMaruPrimitive DiscrSExpr $ AtomSymbol "set") modifiedEnv
-- >>> (Right sexpr, env, _) <- flip runMaruEvaluator modifiedEnv' $ get [AtomSymbol "*x*"]
-- >>> sexpr
-- AtomInt 10
-- >>> (Right sexpr, _, _) <- flip runMaruEvaluator modifiedEnv' $ find [AtomSymbol "*f*"]
-- >>> sexpr == AtomSymbol "set"
-- True
-- >>> (evalResult, env, _) <- flip runMaruEvaluator modifiedEnv' $ get [AtomSymbol "this-is-an-undefined-variable"]
-- >>> isLeft evalResult
-- True
get :: MaruMacro
get [] = fail "get: requires non empty arguments"
get (AtomSymbol sym:_) = do
  value <- find [AtomSymbol sym]
  case value of
    Nil -> fail . T.unpack . unMaruSymbol $ "get: A symbol '" <> sym <> "' is not found in the current environment"
    _   -> return value
get xs = fail $ "get: an invalid condition is detected `" ++ show xs ++ "`"
