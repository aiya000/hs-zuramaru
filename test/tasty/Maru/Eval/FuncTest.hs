{-# LANGUAGE OverloadedStrings #-}

module Maru.Eval.FuncTest where

import Control.Exception.Safe (SomeException)
import Maru.Type (SExpr(..), MaruEnv, SimplificationSteps)
import MaruTest (shouldBeEvaluatedTo)
import System.IO.Silently (silence)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=), Assertion, assertFailure)
import qualified Maru.Eval as Eval

test_evaluator_calculates_pure_functions :: [TestTree]
test_evaluator_calculates_pure_functions =
  [ testCase "(+ 2 3) to 5" $
      Cons (AtomSymbol "+") (Cons (AtomInt 2) (Cons (AtomInt 3) Nil))
      !?= AtomInt 5
  , testCase "(+ 2 (* 3 4)) to 14" $
      Cons (AtomSymbol "+") (Cons (AtomInt 2) (Cons (Cons (AtomSymbol "*") (Cons (AtomInt 3) (Cons (AtomInt 4) Nil))) Nil))
      !?= AtomInt 14
  , testCase "(+ 1 2 3) to 6" $
      Cons (AtomSymbol "+") (Cons (AtomInt 1)
                            (Cons (AtomInt 2)
                            (Cons (AtomInt 3)
                            Nil)))
      !?= AtomInt 6
  ]


test_evaluator_evaluates_four_arith_operations :: [TestTree]
test_evaluator_evaluates_four_arith_operations =
  [ testCase "`+` adds the tail numeric elements to the head numeric element" $ do
      "(+ 1 2 3)" `shouldBeEvaluatedTo` "6"
      "(+ 1)" `shouldBeEvaluatedTo` "1"
  , testCase "`-` substracts the tail numeric elements from the head numeric element" $ do
      "(- 10 2 3)" `shouldBeEvaluatedTo` "5"
      "(- 0 2 3)" `shouldBeEvaluatedTo` "-5"
      "(- 10)" `shouldBeEvaluatedTo` "-10"
  , testCase "`*` adds the numeric element to itself the {element} times foldly" $
      "(* 2 3 4)" `shouldBeEvaluatedTo` "24"
  , testCase "`/` gets the inverse value of the integral element foldly" $ do
      "(/ 4 2 2)" `shouldBeEvaluatedTo` "1"
  ]


(!?=) :: SExpr -> SExpr -> Assertion
origin !?= expected = do
  result <- evalInitSilently origin
  case result of
    Left e -> assertFailure $ "An error is caught: " ++ show e
    Right (actual, _, _) -> actual @?= expected
  where
    evalInitSilently :: SExpr -> IO (Either SomeException (SExpr, MaruEnv, SimplificationSteps))
    evalInitSilently = silence . Eval.eval Eval.initialEnv
