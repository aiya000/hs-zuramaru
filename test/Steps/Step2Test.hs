{-# LANGUAGE OverloadedStrings #-}

module Steps.Step2Test where

import Data.Text (Text)
import Maru.Type (SExpr(..), SExprLike(..))
import System.IO.Silently (silence)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=), Assertion, assertFailure)
import qualified Maru.Eval as Eval


-- | See https://github.com/kanaka/mal/blob/master/process/guide.md#step-2-eval
test_evaluator_evaluates :: [TestTree]
test_evaluator_evaluates =
  [ testCase "(+ 2 3) to 5" $
      Cons (AtomSymbol "+") (Cons (AtomInt 2) (Cons (AtomInt 3) Nil))
      !?= AtomInt 5
  , testCase "(+ 2 (* 3 4)) to 14" $
      Cons (AtomSymbol "+") (Cons (AtomInt 2)
                            (Cons (AtomSymbol "*") (Cons (AtomInt 3)
                                                   (Cons (AtomInt 4)
                                                   Nil))))
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
      , testCase "(1 2) (naked expression) to an error" $ do
          result <- evalInitSilently $ Cons (AtomInt 1) (Cons (AtomInt 2) Nil)
          case result of
            Left e -> return () -- The naked expression should be the error
            Right a -> assertFailure $ "A right result is detected, but a naked expression should be an error: " ++ show a
      ]


evalInitSilently :: SExpr -> IO (Either SomeException (SExpr, MaruEnv))
evalInitSilently = silence . Eval.eval Eval.initialEnv


(!?=) :: SExpr -> SExpr -> Assertion
origin !?= expected = do
  result <- evalInitSilently origin
  case result of
    Left e -> assertFailure $ "An error is caught: " ++ show e
    Right (actual, _) -> actual @?= expected
