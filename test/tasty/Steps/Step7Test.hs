{-# LANGUAGE OverloadedStrings #-}

module Steps.Step7Test where

import Maru.Type
import MaruTest
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Maru.Parser as P
import qualified Maru.Preprocessor as Pre

test_quote_prefix :: [TestTree]
test_quote_prefix =
  [ testCase "(e.g. `'1') is parsed to 'Quote (AtomInt 1)`" $ do
      P.parse "'1" @?= Right (Quote' (AtomInt' 1))
      P.parse "'a" @?= Right (Quote' (AtomSymbol' "a"))
      P.parse "'(1 2)" @?= Right (Quote' (Cons' (AtomInt' 1) (Cons' (AtomInt' 2) Nil')))
  ]

test_quote_symbol :: [TestTree]
test_quote_symbol =
  [ testCase "(e.g. 'Cons (AtomSymbol \"quote\") (Cons x Nil)') is preprocessed to 'Quote x'" $ do
      let x  = AtomInt 1
          x' = CallowSExpr x
      Pre.preprocess (Cons' (AtomSymbol' "quote") (Cons' x' Nil')) @?= Quote x
      let xs  = Cons (AtomInt 1) (Cons (AtomInt 2) Nil)
          xs' = CallowSExpr xs
      Pre.preprocess (Cons' (AtomSymbol' "quote") (Cons' xs' Nil')) @?= Quote xs
  ]

test_quote_macro :: [TestTree]
test_quote_macro =
  [ testCase "delays the evaluation of an element" $ do
      "(quote (1 2 3))" `shouldBeEvaluatedTo` "(1 2 3)"
      "(quote (quote 2))" `shouldBeEvaluatedTo` "(quote 2)"
      "(quote (10 (quote 20)))" `shouldBeEvaluatedTo` "(10 (quote 20))"
  , testCase "is meant by `'` prefix" $ do
      "'(1 2 3)" `shouldBeEvaluatedTo` "(1 2 3)"
      "'1" `shouldBeEvaluatedTo` "(quote 1)"
      "''2" `shouldBeEvaluatedTo` "(quote (quote 2))"
  ]
