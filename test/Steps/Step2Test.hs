{-# LANGUAGE OverloadedStrings #-}

module Steps.Step2Test where

import Control.Exception.Safe (SomeException)
import Maru.Type (SExpr(..), MaruEnv, SimplificationSteps)
import System.IO.Silently (silence)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=), Assertion, assertFailure)
import qualified Maru.Eval as Eval


-- | See https://github.com/kanaka/mal/blob/master/process/guide.md#step-2-eval
test_evaluator_calculates :: [TestTree]
test_evaluator_calculates =
  [ testCase "(+ 2 3) to 5" $
      Cons (AtomSymbol "+") (Cons (AtomInt 2) (Cons (AtomInt 3) Nil))
      !?= AtomInt 5
  , testCase "(+ 2 (* 3 4)) to 14" $
      Cons (AtomSymbol "+") (Cons (AtomInt 2) (Cons (Cons (AtomSymbol "*") (Cons (AtomInt 3) (Cons (AtomInt 4) Nil))) Nil))
      !?= AtomInt 14
  ] ++ myAddtionalCases
  where
    myAddtionalCases :: [TestTree]
    myAddtionalCases =
      [ testCase "(+ 1 2 3) to 6" $
          Cons (AtomSymbol "+") (Cons (AtomInt 1)
                                (Cons (AtomInt 2)
                                (Cons (AtomInt 3)
                                Nil)))
          !?= AtomInt 6
      ]


evalInitSilently :: SExpr -> IO (Either SomeException (SExpr, MaruEnv, SimplificationSteps))
evalInitSilently = silence . Eval.eval Eval.initialEnv


(!?=) :: SExpr -> SExpr -> Assertion
origin !?= expected = do
  result <- evalInitSilently origin
  case result of
    Left e -> assertFailure $ "An error is caught: " ++ show e
    Right (actual, _, _) -> actual @?= expected
