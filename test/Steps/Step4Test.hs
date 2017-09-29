{-# LANGUAGE OverloadedStrings #-}

module Steps.Step4Test where

import Maru.Parser (parse)
import Maru.Type (readable, SExpr(..))
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
  ] ++ readable_func_endomorphism_test


-- FIXME: I write below doctests as `readable`'s document comment, but the fcking error is happened. why? I write this test because is is happened. Please fix this, oh my god.
-- >>> readable <$> parse "10"
-- Right "10"
-- >>> parse . readable $ AtomInt 10
-- Right (AtomInt 10)
--
-- >>> readable <$> parse "(+ 1 2)"
-- Right "(+ 1 2)"
-- >>> let result = parse . readable $ Cons (AtomSymbol "+") (Cons (AtomInt 1) (Cons (AtomInt 2) Nil))
-- >>> result == Right (Cons (AtomSymbol "+") (Cons (AtomInt 1) (Cons (AtomInt 2) Nil)))
-- True
readable_func_endomorphism_test :: [TestTree]
readable_func_endomorphism_test =
  [ testCase "`readable` and `parse` are endomorphic if the format is ignored" $ do
      (readable <$> parse "10")
        @?= Right "10"
      parse (readable $ AtomInt 10)
        @?= Right (AtomInt 10)
      readable <$> parse "(+ 1 2)"
        @?= Right "(+ 1 2)"
      parse (readable $ Cons (AtomSymbol "+") (Cons (AtomInt 1) (Cons (AtomInt 2) Nil)))
        @?= Right (Cons (AtomSymbol "+") (Cons (AtomInt 1) (Cons (AtomInt 2) Nil)))
  ]
