{-# LANGUAGE OverloadedLabels #-}

module Maru.Type.EvalTest where

import Control.Monad.Fail (fail)
import Maru.Type.Eval (MaruCalculator, runMaruCalculator, throwFail)
import Prelude hiding (fail)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Data.Text as T


oops :: String
oops = "Oops xD"


test_maru_calculator_throws_the_exception_as_a_pure_value_by_fail :: [TestTree]
test_maru_calculator_throws_the_exception_as_a_pure_value_by_fail =
  [ testCase "" $
      runMaruCalculator (negativeContext oops)
        @?= runMaruCalculator (fail' oops)
  ]
  where
    fail' :: String -> MaruCalculator ()
    fail' = throwFail . T.pack


-- | An alias of `fail`
negativeContext :: String -> MaruCalculator ()
negativeContext = fail
