

module Maru.Type.EvalTest where

import Data.Either (isLeft)
import Maru.Eval (initialEnv)
import Maru.Type (MaruEvaluator, runMaruEvaluator)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?))


test_MaruEvaluator_fail_is_pure :: [TestTree]
test_MaruEvaluator_fail_is_pure =
  [ testCase "fail is same as throwExc" $ do
      (result, _, _) <- runMaruEvaluator failureContext initialEnv
      isLeft result @? "This expression should be executed" -- This maybe executed if MaruEvaluator's fail is not impure
  ]
  where
    failureContext :: MaruEvaluator ()
    failureContext = fail "failure in MaruEvaluator context"
