-- |
-- Define functions and macros, these are used in the runtime,
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

import Control.Lens ((^..), folded, filtered, sumOf)
import Control.Monad.Fail (fail)
import Data.MonoTraversable (omap)
import Maru.Type (MaruMacro, MaruFunc, SExpr(..), SomeMaruPrimitive(..), Discriminating(..), SExprIntBullet(..), Result(..))
import Prelude hiding (div, fail)
import qualified Control.Eff.State.Lazy as STL
import qualified Data.Map.Lazy as M
import qualified Maru.Type as MT


ignoreAtomInt :: [SExpr] -> [SExpr]
ignoreAtomInt xs = xs ^.. folded . filtered (not . MT.isAtomInt)


-- |
-- Find all `AtomInt` elements,
-- calculate its summation,
-- and wrap by `AtomInt`.
--
-- Return `AtomInt 0` if the given list is empty.
sumOfAtomInt :: [SExpr] -> SExpr
sumOfAtomInt = (AtomInt .) . sumOf $ folded . MT._AtomInt


-- |
-- >>> :set -XOverloadedStrings
-- >>> add [AtomInt 1, AtomInt 2]
-- Result {unResult = Right (AtomInt 3)}
-- >>> add []
-- Result {unResult = Right (AtomInt 0)}
-- >>> add [AtomSymbol "xD"]
-- Result {unResult = Left "add: invalid arguments are given to (+): [AtomSymbol (Symbol {unSymbol = \"xD\"})]"}
add :: MaruFunc
add xs = case ignoreAtomInt xs of
  [] -> return $ sumOfAtomInt xs
  invalidArgs -> fail $ "add: invalid arguments are given to (+): " ++ show invalidArgs


-- |
-- >>> :set -XOverloadedStrings
-- >>> sub []
-- Result {unResult = Left "sub takes integer values, but took arguments is empty"}
-- >>> sub [AtomInt 3, AtomInt 1]
-- Result {unResult = Right (AtomInt 2)}
-- >>> sub [AtomSymbol "xD"]
-- Result {unResult = Left "sub: invalid arguments are given to (-): [AtomSymbol (Symbol {unSymbol = \"xD\"})]"}
sub :: MaruFunc
sub [] = fail "sub takes integer values, but took arguments is empty"
sub xs = case ignoreAtomInt xs of
  [] -> return $ negativeSumOfAtomInt xs
  invalidArgs -> fail $ "sub: invalid arguments are given to (-): " ++ show invalidArgs
  where
    -- head - tail
    negativeSumOfAtomInt :: [SExpr] -> SExpr
    negativeSumOfAtomInt (x:xs) = sumOfAtomInt . (x:) . flip map xs $ MT.intBullet negate


times :: MaruFunc
times = undefined


div :: MaruFunc
div = undefined


--NOTE: CLisp's defparameter returns the symbol, it is a defined symbol. This is respecting it.
--TODO: Correspond for the type of other than Int after the type is added to SExpr and somewhere
set :: MaruMacro
set [] = fail "set: requires non empty arguments"
set (AtomSymbol sym:AtomInt x:xs) = do
  env <- STL.get
  STL.put $ M.insert sym (SomeMaruPrimitive DiscrInt x) env
  return $ AtomSymbol sym
set xs = fail $ "set: an invalid condition is detected `" ++ show xs ++ "`"
--set xs = fail $ "set: requires a symbol as a head element but a form of `" ++ show x "` is detected"


find :: MaruMacro
find = undefined


get :: MaruMacro
get = undefined
