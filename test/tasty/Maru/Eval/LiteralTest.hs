{-# LANGUAGE OverloadedStrings #-}

module Maru.Eval.LiteralTest where

import MaruTest
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)


test_boolean_literals :: [TestTree]
test_boolean_literals =
  [ testCase "`true` literal is printable" $ do
      "true" `shouldBeEvaluatedTo` "true"
      "(def! x true)" `shouldBeEvaluatedTo` "true"
  , testCase "`false` literal is printable" $ do
      "false" `shouldBeEvaluatedTo` "false"
      "(def! x false)" `shouldBeEvaluatedTo` "false"
  ]


test_nil_literal :: [TestTree]
test_nil_literal =
  [ testCase "`nil` is evaluated to `()`" $
      "nil" `shouldBeEvaluatedTo` "()"
  ]


test_integral_positive_literals :: [TestTree]
test_integral_positive_literals =
  [ testCase "can be evaluated" $ do
      "+1" `shouldBeEvaluatedTo` "1"
      "(+ +1 2)" `shouldBeEvaluatedTo` "3"
      "(+ 2 +1)" `shouldBeEvaluatedTo` "3"
  ]

test_integral_negative_literals :: [TestTree]
test_integral_negative_literals =
  [ testCase "can be evaluated" $ do
      "-1" `shouldBeEvaluatedTo` "-1"
      "(+ -1 2)" `shouldBeEvaluatedTo` "1"
      "(+ 2 -1)" `shouldBeEvaluatedTo` "1"
  ]
