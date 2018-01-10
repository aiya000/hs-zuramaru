{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Steps.Step4Test where

import Data.Semigroup ((<>))
import Maru.Type (SExpr(..), readable)
import MaruTest
import System.IO.Silently (capture_)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import qualified Maru.Eval as E


test_do_macro :: [TestTree]
test_do_macro =
  [ testCase "evaluates taken arguments" $ do
      (sexpr, env, _) <- runCodeInstantly $ "(do (def! x 10)" <>
                                              "(def! y (+ x 1))" <>
                                              "(def! z (+ y 1)))"
      readable sexpr @?= "12"
      "x" `isExistedIn` env
      "y" `isExistedIn` env
      "z" `isExistedIn` env
  ]


test_if_macro :: [TestTree]
test_if_macro =
  [ testCase "evaluates the third argument and returns the result, if the first argument is evaluated to `false` or `nil`" $ do
      "(if false 0 1)" `shouldBeEvaluatedTo` "1"
      "(if nil 0 3)" `shouldBeEvaluatedTo` "3"

      [ "(def! x false)"
       ,"(if x 0 5)"
       ] `shouldBeEvaluatedTo'` "5"

      [ "(def! x ())"
       ,"(if x 0 7)"
       ] `shouldBeEvaluatedTo'` "7"

  , testCase "evaluates the second argument and returns the result, if the first argument is evaluated to neither `false` nor `nil`" $ do
      "(if true 1 0)" `shouldBeEvaluatedTo` "1"
      "(if 0 5 0)" `shouldBeEvaluatedTo` "5"
      "(if 10 7 0)" `shouldBeEvaluatedTo` "7"

      (_, env, _) <- runCodeInstantly "(def! x true)"
      (sexpr, _, _) <- runCode env "(if x 9 0)"
      sexpr @?= AtomInt 9
  ]


test_fn_macro :: [TestTree]
test_fn_macro =
  [ testCase "can be applied with arguments" $ do
      "((fn* (a) 10) 0)" `shouldBeEvaluatedTo` "10" -- an argument
      "((fn* () 10))" `shouldBeEvaluatedTo` "10" -- zero arguments
      "((fn* (x y z) y) 1 2 3)" `shouldBeEvaluatedTo` "2" -- multi arguments

  , testCase "can be bound as the variable" $ do
      -- united
      "(let* (f (fn* (x) x)) (f 10))" `shouldBeEvaluatedTo` "10"
      -- devided
      [ "(def! f (fn* (x) x))"
       ,"(f 10)"
       ] `shouldBeEvaluatedTo'` "10"
      -- nested
      runCorretly $ "(let* (x 10)" <>
                      "(let* (f (fn* (a) x))" <>
                        "(f 0)))"

  , testCase "can include the outer scopes variables (the behavior of closure)" $
      [ "(let* (x 10) (def! f (fn* (a) x)))"
      , "(f 0)"
      ] `shouldBeEvaluatedTo'` "10"
  ]


test_print_macro :: [TestTree]
test_print_macro =
  [ testCase "prints a S expression on the screen" $ do
      captured <- capture_ $ runCodeInstantly "(print 10)"
      captured @?= "10"

      captured <- capture_ $ runCodes E.initialEnv [ "(def! x 10)"
                                                   , "(print x)"
                                                   ]
      captured @?= "10"
  , testCase "prints S expressions on the screen" $ do
      captured <- capture_ $ runCodeInstantly "(print 1 2 3)"
      captured @?= "1\n2\n3"
  , testCase "returns ()" $
      "(print 10)" `shouldBeEvaluatedTo` "()"
  , testCase "prints nothing if anything are not taken" $ do
      captured <- capture_ $ runCodeInstantly "(print)"
      captured @?= ""
  ]


test_list_macro :: [TestTree]
test_list_macro =
  [ testCase "makes a list with the taken arguments" $ do
      "(list)" `shouldBeEvaluatedTo` "()"
      "(list 1 2 3)" `shouldBeEvaluatedTo` "(1 2 3)"
  , testCase "evaluates each arguments" $
      "(list (+ 1 2) (+ 3 4))" `shouldBeEvaluatedTo` "(3 7)"
  ]


test_my_another_things :: [TestTree]
test_my_another_things =
  [ testCase "`(x)` happens an exception (because the form of `(x)` expects `x` is the symbol of the function or the macro)" $ do
      point <- runCodeWithSteps E.initialEnv "(10)"
      case point of
        EvalError _ -> return ()
        x           -> assertFailure $ "expected a `EvalError`, but got `" ++ show x ++ "`"
  ]
