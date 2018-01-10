{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Maru.Eval.MacroTest where

import Data.Semigroup ((<>))
import Maru.Type (SExpr(..), MaruEnv, readable)
import MaruTest
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Maru.Eval as E


-- | def!
test_defBang_macro :: [TestTree]
test_defBang_macro =
  [ testCase "(`def!`) adds a value with a key to environment" $ do
      (sexpr, env, _) <- runCodeInstantly "(def! *poi* 10)"
      readable sexpr @?= "10"
      (poi, _, _) <- runCode env "*poi*"
      readable poi @?= "10"
  ]


-- | let*
test_letStar_macro :: [TestTree]
test_letStar_macro =
  [ testCase "(`let*`) adds a value with akey to new environment scope" $
      "(let* (x 10) x)" `shouldBeEvaluatedTo` "10"
  ]


-- |
-- e.g. (+ 1 2), *y* to be called by `call`
-- (regard that *y* is set)
test_call :: [TestTree]
test_call =
  [ testCase "calls a first element of the list as a function/macro with tail elements implicitly" $ do
      "(+ 1 2)" `shouldBeEvaluatedTo` "3"
      (x, _, _) <- runCode modifiedEnv "*x*"
      readable x @?= "10"
      (y, _, _) <- runCode modifiedEnv "*y*"
      readable y @?= "10"
  ]
  where
    -- initialEnv ∪ { (*x* := 10), (*x* := *x*) }
    modifiedEnv :: MaruEnv
    modifiedEnv = E.initialEnv <>
                    [[ ("*x*", AtomInt 10)
                     , ("*y*", AtomSymbol "*x*")
                     ]]


test_lexical_scope :: [TestTree]
test_lexical_scope =
  [ testCase "The lexical scope behavior is correct" $ do
      (sexpr, env, _) <- runCodeInstantly "(let* (x 10) x)"
      -- the internal operation takes "x" well
      readable sexpr @?= "10"
      -- "x" cannot be gotten in the outer scope
      "x" `isNotExistedIn` env
  ]
