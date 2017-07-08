{-# LANGUAGE OverloadedStrings #-}

module Steps.Step2Test where

import Maru.Type (SExpr(..), symbol, int)
import System.IO.Silently (silence)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import qualified Maru.Eval as Eval


-- | See https://github.com/kanaka/mal/blob/master/process/guide.md#step-2-eval
test_evaluator_evaluates :: [TestTree]
test_evaluator_evaluates =
  [ testCase "(+ 2 3) to 5" $
      Cons (symbol "+") (Cons (int 2) (Cons (int 3) Nil)) !?= int 5
  , testCase "(+ 2 (* 3 4)) to 14" $
      Cons (symbol "+") (Cons (int 2)
                        (Cons (symbol "*") (Cons (int 3)
                                           (Cons (int 4) Nil)
                        )))
                        !?= int 14
  ]
  where
    (!?=) :: SExpr -> SExpr -> Assertion
    origin !?= expected = do
      (actual, _) <- silence $ Eval.eval Eval.initialEnv origin
      actual @?= expected
