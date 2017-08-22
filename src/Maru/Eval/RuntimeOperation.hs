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
import Maru.Type (MaruMacro, MaruFunc, SExpr(..), SomeMaruPrimitive(..), Discriminating(..), SExprIntBullet(..), MaruCalculator, runMaruCalculator)
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
-- >>> runMaruCalculator $ add [AtomInt 1, AtomInt 2]
-- Right (AtomInt 3)
-- >>> runMaruCalculator $ add []
-- Right (AtomInt 0)
-- >>> runMaruCalculator $ add [AtomSymbol "xD"]
-- Left "add: invalid arguments are given to (+): [AtomSymbol (Symbol {unSymbol = \"xD\"})]"
add :: MaruFunc
add xs = case ignoreAtomInt xs of
  [] -> return $ sumOfAtomInt xs
  invalidArgs -> fail $ "add: invalid arguments are given to (+): " ++ show invalidArgs


-- |
-- >>> :set -XOverloadedStrings
-- >>> runMaruCalculator $ sub [AtomInt 3, AtomInt 1]
-- Right (AtomInt 2)
-- >>> runMaruCalculator $ sub []
-- Left "sub: takes a list of integer values, but took list is empty"
-- >>> runMaruCalculator $ sub [AtomSymbol "xD"]
-- Left "sub: invalid arguments are given to (-): [AtomSymbol (Symbol {unSymbol = \"xD\"})]"
sub :: MaruFunc
sub [] = fail "sub: takes a list of integer values, but took list is empty"
sub xs = case ignoreAtomInt xs of
  [] -> return $ negativeSumOfAtomInt xs
  invalidArgs -> fail $ "sub: invalid arguments are given to (-): " ++ show invalidArgs
  where
    -- head - tail
    negativeSumOfAtomInt :: [SExpr] -> SExpr
    negativeSumOfAtomInt (x:xs) = sumOfAtomInt . (x:) . flip map xs $ MT.intBullet negate


-- |
-- >>> :set -XOverloadedStrings
-- >>> runMaruCalculator $ times [AtomInt 3, AtomInt 3]
-- Right (AtomInt 9)
-- >>> runMaruCalculator $ times []
-- Right (AtomInt 1)
-- >>> runMaruCalculator $ times [AtomSymbol "xD"]
-- Left "times: invalid arguments are given to (*): [AtomSymbol (Symbol {unSymbol = \"xD\"})]"
times :: MaruFunc
times = undefined


--TODO: This makes an integral number unless like AtomRatio is implemented to SExpr
-- |
-- >>> :set -XOverloadedStrings
-- >>> runMaruCalculator $ div [AtomInt 3, AtomInt 3]
-- Right (AtomInt 1)
-- >>> runMaruCalculator $ div []
-- Left "div: takes a non empty list, but took list is empty"
-- >>> runMaruCalculator $ div [AtomInt 0, AtomInt 1]
-- Left "div: 0 is divided by anything"
-- >>> runMaruCalculator $ div [AtomInt 10, AtomInt 3]
-- Right (AtomInt 3)
-- >>> runMaruCalculator $ div [AtomInt 3, AtomInt 5]
-- Right (AtomInt 0)
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
