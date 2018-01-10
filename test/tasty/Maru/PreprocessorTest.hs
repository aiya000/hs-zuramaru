{-# LANGUAGE OverloadedStrings #-}

module Maru.PreprocessorTest where

import Maru.Type
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Maru.Preprocessor as Pre

test_preprocessor_fixes_quote_symbols :: [TestTree]
test_preprocessor_fixes_quote_symbols =
  [ testCase "(e.g. 'Cons (AtomSymbol \"quote\") (Cons x Nil)') is preprocessed to 'Quote x'" $ do
      let x  = AtomInt 1
          x' = CallowSExpr x
      Pre.preprocess (Cons' (AtomSymbol' "quote") (Cons' x' Nil')) @?= Quote x
      let xs  = Cons (AtomInt 1) (Cons (AtomInt 2) Nil)
          xs' = CallowSExpr xs
      Pre.preprocess (Cons' (AtomSymbol' "quote") (Cons' xs' Nil')) @?= Quote xs
  ]

