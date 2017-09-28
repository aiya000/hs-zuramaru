{-# LANGUAGE OverloadedStrings #-}

module Steps.Step4Test where

import Maru.Type (readable)
import MaruTest (runCodeInstantly)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

test_boolean_literals :: [TestTree]
test_boolean_literals =
  [ testCase "`true` literal is printable" $ do
      let expected = "true"
      (sexpr, _, _) <- runCodeInstantly expected
      readable sexpr @?= expected
  , testCase "`false` literal is printable" $ do
      let expected = "false"
      (sexpr, _, _) <- runCodeInstantly expected
      readable sexpr @?= expected
  ]
