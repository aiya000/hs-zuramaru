{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Define functions and macros, these are used in the runtime,
--
-- also these are through with `MaruEnv` (it should be `Maru.Eval.initialEnv`)
--
-- These respects clisp's behavior basically.
--
-- `add`, `sub`, `times`, and `div` are regarded as axiomly functions.
module Maru.Eval.RuntimeOperation
  ( add
  , sub
  , times
  , div
  ) where

import Control.Lens hiding (set)
import Control.Monad.Fail (fail)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (maybeToList)
import Maru.Type
import Numeric.Extra (intToDouble)
import Prelude hiding (div, fail)
import qualified Data.List.NonEmpty as NE
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
-- >>> runMaruCalculator $ execFunc add [AtomInt 1, AtomInt 2]
-- Right (AtomInt 3)
-- >>> runMaruCalculator $ execFunc add []
-- Right (AtomInt 0)
-- >>> isLeft . runMaruCalculator $ execFunc add [AtomSymbol "xD"]
-- True
add :: MaruFunc
add = MaruFunc $ \xs -> case ignoreAtomInt xs of
  [] -> return $ sumOfAtomInt xs
  invalidArgs -> fail $ "add: invalid arguments are given to (+): " ++ show invalidArgs


-- |
-- >>> runMaruCalculator $ execFunc sub [AtomInt 3, AtomInt 1]
-- Right (AtomInt 2)
-- >>> isLeft . runMaruCalculator $ execFunc sub []
-- True
-- >>> isLeft . runMaruCalculator $ execFunc sub [AtomSymbol "xD"]
-- True
sub :: MaruFunc
sub = MaruFunc $ \case
  [] -> fail "sub: takes a list of integer values, but took list is empty"
  w@(x:xs) -> case ignoreAtomInt w of
    [] -> return $ negativeSumOfAtomInt (x:|xs)
    invalidArgs -> fail $ "sub: invalid arguments are given to (-): " ++ show invalidArgs
  where
    -- head - tail
    negativeSumOfAtomInt :: NonEmpty SExpr -> SExpr
    negativeSumOfAtomInt (x:|xs) = sumOfAtomInt . (x:) . flip map xs $ MT.intBullet negate


-- |
-- >>> runMaruCalculator $ execFunc times [AtomInt 3, AtomInt 3]
-- Right (AtomInt 9)
-- >>> runMaruCalculator $ execFunc times []
-- Right (AtomInt 1)
-- >>> isLeft . runMaruCalculator $ execFunc times [AtomSymbol "xD"]
-- True
times :: MaruFunc
times = MaruFunc $ \xs -> case ignoreAtomInt xs of
  [] -> return . AtomInt $ productOf (folded . MT._AtomInt) xs
  invalidArgs -> fail $ "times: invalid arguments are given to (*): " ++ show invalidArgs


--TODO: This makes an integral number unless like AtomRatio is implemented to SExpr
-- |
-- >>> runMaruCalculator $ execFunc div [AtomInt 3, AtomInt 3]
-- Right (AtomInt 1)
-- >>> isLeft . runMaruCalculator $ execFunc div []
-- True
-- >>> isLeft . runMaruCalculator $ execFunc div [AtomSymbol "xD"]
-- True
-- >>> isLeft . runMaruCalculator $ execFunc div [AtomInt 0, AtomInt 1]
-- True
-- >>> runMaruCalculator $ execFunc div [AtomInt 10, AtomInt 3]
-- Right (AtomInt 3)
-- >>> runMaruCalculator $ execFunc div [AtomInt 3, AtomInt 5]
-- Right (AtomInt 0)
div :: MaruFunc
div = MaruFunc $ \case
  [] -> fail "div: takes a non empty list, but took list is empty"
  w@(x:xs) -> case (ignoreAtomInt w, negativeProductOfAtomInt (x:|xs)) of
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
