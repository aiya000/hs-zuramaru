{-# LANGUAGE OverloadedStrings #-}

module Steps.Step4Test where

import Control.Lens (view, _2)
import Data.Semigroup ((<>))
import Maru.Type (SExpr(..))
import MaruTest
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import qualified Maru.Eval as E
import qualified Maru.Parser as P


test_boolean_literals :: [TestTree]
test_boolean_literals =
  [ testCase "`true` literal is printable" $ do
      "true"          `shouldBeEvaluatedTo` "true"
      "(def! x true)" `shouldBeEvaluatedTo` "true"
  , testCase "`false` literal is printable" $ do
      "false"          `shouldBeEvaluatedTo` "false"
      "(def! x false)" `shouldBeEvaluatedTo` "false"
  ]


test_nil_literal :: [TestTree]
test_nil_literal =
  [ testCase "`nil` equals `()`" $
      "nil" `shouldBeEvaluatedTo` "()"
  ]


-- | My additional test
test_integral_positive_literals :: [TestTree]
test_integral_positive_literals =
  [ testCase "can be evaluated" $ do
      "+1"       `shouldBeEvaluatedTo` "1"
      "(+ +1 2)" `shouldBeEvaluatedTo` "3"
      "(+ 2 +1)" `shouldBeEvaluatedTo` "3"
  ]

-- | My additional test
test_integral_negative_literals :: [TestTree]
test_integral_negative_literals =
  [ testCase "can be evaluated" $ do
      "-1"       `shouldBeEvaluatedTo` "-1"
      "(+ -1 2)" `shouldBeEvaluatedTo` "1"
      "(+ 2 -1)" `shouldBeEvaluatedTo` "1"
  ]


test_do_macro :: [TestTree]
test_do_macro =
  [ testCase "evaluates taken arguments" $ do
      (sexpr, env, _) <- runCodeInstantly $ "(do (def! x 10)" <>
                                            "    (def! y (+ x 1))" <>
                                            "    (def! z (+ y 1)))"
      sexpr @?= AtomInt 12
      "x" `isExistedIn` env
      "y" `isExistedIn` env
      "z" `isExistedIn` env
  ]


test_if_macro :: [TestTree]
test_if_macro =
  [ testCase "evaluates the third argument and returns the result, if the first argument is evaluated to `false` or `nil`" $ do
      "(if false 0 1)" `shouldBeEvaluatedTo` "1"
      "(if () 0 3)"    `shouldBeEvaluatedTo` "3"

      (_, env, _) <- runCodeInstantly "(def! x false)"
      (sexpr, _, _) <- runCode env "(if x 0 5)"
      sexpr @?= AtomInt 5

      (_, env, _) <- runCodeInstantly "(def! x ())"
      (sexpr, _, _) <- runCode env "(if x 0 7)"
      sexpr @?= AtomInt 7

  , testCase "evaluates the second argument and returns the result, if the first argument is evaluated to neither `false` nor `nil`" $ do
      "(if true 1 0)" `shouldBeEvaluatedTo` "1"
      "(if 0 5 0)"    `shouldBeEvaluatedTo` "5"
      "(if 10 7 0)"   `shouldBeEvaluatedTo` "7"

      (_, env, _) <- runCodeInstantly "(def! x true)"
      (sexpr, _, _) <- runCode env "(if x 9 0)"
      sexpr @?= AtomInt 9
  ]


test_fn_macro :: [TestTree]
test_fn_macro =
  [ testCase "only expands a S expression of its body with `expanded-fn*`" $ do
      (sexpr, _, _) <- runCodeInstantly "(def! z 1)"
                       >>= flip runCode "(def! y (- 1 z))" . view _2
                       >>= flip runCode "(def! x (+ y z))" . view _2
                       >>= flip runCode "(fn* (a) x)" . view _2
      --case P.parse "(expanded-fn* (a) (+ (- 1 1) 1))" of
      --TODO: `x` is expanded to `1` (not `(+ (- 1 1) 1)`),
      --      because `def!` evaluates terms at now (`fn*` doesn't evaluate terms).
      --      Determine about do implement the lazy evaluation on `def!` (or don't)
      case P.parse "(expanded-fn* (a) 1)" of
        Left  e -> assertFailure $ show e
        Right a -> sexpr @?= a
  , testCase "cannot take zero arguments" $ do
      point <- runCodeWithSteps E.initialEnv "((fn* () 10) 0)"
      case point of
        EvalError _ -> return ()
        x           -> assertFailure $ "expected a `EvalError`, but got `" ++ show x ++ "`"
  , testCase "can take multi arguments" $
      "(fn* (x y z) 0)" `shouldBeEvaluatedTo` "(expanded-fn* (x y z) 0)"
  , testCase "can be applied with arguments" $
      "((fn* (a) 10) 0)" `shouldBeEvaluatedTo` "10"
  ]


-- |
-- `expanded-fn*` is not used in the mostly cases (it is used via `fn*`),
-- because it throws the exception if symbols are not found in its body.
--
-- e.g.
-- `
-- (let* (x 10)
--    ((expanded-fn* (a) x) 0))
-- `
-- ,
-- `
-- (let* (x 10)
--    (let* (f (expanded-fn* (a) x))
--        (f 0)))
-- `
-- and
-- `
-- (let* (x 10)
--    (def! f (fn* (a) x))) ; x is expanded by fn*
-- (f 0)
-- `
-- to be success, but
-- `
-- (let* (x 10)
--    (def! f (expanded-fn* (a) x))) ; expanded-fn* doesn't expand x
-- (f 0)
-- `
-- to be failure.
test_expanded_fn_macro :: [TestTree]
test_expanded_fn_macro =
  [ testCase "is applied with arguments as a function without any binding" $ do
      "((expanded-fn* (x) x) 10)" `shouldBeEvaluatedTo` "10"

  , testCase "is applied with arguments as a function with any binding" $ do
      ( "(let* (x 10)" <>
          "(let* (f (expanded-fn* (a) x))" <>
            "(f 0)))" ) `shouldBeEvaluatedTo` "10"

  , testCase "can include variables of outer scopes with `fn*` (the behavior of closure)" $ do
      (sexpr, _, _) <- runCodeInstantly "(let* (x 10) (def! f (fn* (a) x)))"
                       >>= flip runCode "(f 0)" . view _2
      sexpr @?= AtomInt 10
  ]


test_my_another_things :: [TestTree]
test_my_another_things =
  [ testCase "`(x)` happens an exception (because the form of `(x)` expects `x` is the symbol of the function or the macro)" $ do
      point <- runCodeWithSteps E.initialEnv "(10)"
      case point of
        EvalError _ -> return ()
        x           -> assertFailure $ "expected a `EvalError`, but got `" ++ show x ++ "`"
  ]
