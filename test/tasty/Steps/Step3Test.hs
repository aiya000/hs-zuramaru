{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Steps.Step3Test where

import Control.Lens
import Data.Semigroup ((<>))
import Maru.Type (SExpr(..), MaruEnv)
import MaruTest (runCodeInstantly, runCode)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Maru.Eval as E
import qualified Maru.Type.Eval as E


-- | def!
test_defBang_macro :: [TestTree]
test_defBang_macro =
  [ testCase "(`def!`) adds a value with a key to environment" $ do
      (sexpr, env, _) <- runCodeInstantly "(def! *poi* 10)"
      sexpr @?= AtomInt 10
      env ^? to (E.lookup "*poi*") . _Just
        @?= Just (AtomInt 10)
  ]


-- | let*
test_letStar_macro :: [TestTree]
test_letStar_macro =
  [ testCase "(`let*`) adds a value with akey to new environment scope" $ do
      (sexpr, _, _) <- runCodeInstantly "(let* (x 10) x)"
      sexpr @?= AtomInt 10
  ]


-- |
-- e.g. (+ 1 2), *y* to be called by `call`
-- (regard that *y* is set)
test_call :: [TestTree]
test_call =
  [ testCase "calls a first element of the list as a function/macro with tail elements implicitly" $ do
      (sexpr, _, _) <- runCodeInstantly "(+ 1 2)"
      sexpr @?= AtomInt 3
      (sexpr, _, _) <- runCode modifiedEnv "*x*"
      sexpr @?= AtomInt 10
      (sexpr, _, _) <- runCode modifiedEnv "*y*"
      sexpr @?= AtomInt 10
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
      (result, env, _) <- runCodeInstantly "(let* (x 10) x)"
      -- the internal operation takes "x" well
      result @?= AtomInt 10
      -- "x" cannot be gotten in the outer scope
      E.lookup "x" env ^? _Just
        @?= Nothing
  ]


test_four_arith_operations :: [TestTree]
test_four_arith_operations =
  [ testCase "`+` adds the tail numeric elements to the head numeric element" $ do
      (sexpr, _, _) <- runCodeInstantly "(+ 1 2 3)"
      sexpr @?= AtomInt 6
      (sexpr, _, _) <- runCodeInstantly "(+ 1)"
      sexpr @?= AtomInt 1
  , testCase "`-` substracts the tail numeric elements from the head numeric element" $ do
      (sexpr, _, _) <- runCodeInstantly "(- 10 2 3)"
      sexpr @?= AtomInt 5
      (sexpr, _, _) <- runCodeInstantly "(- 0 2 3)"
      sexpr @?= AtomInt (-5)
      (sexpr, _, _) <- runCodeInstantly "(- 10)"
      sexpr @?= AtomInt (-10)
  , testCase "`*` adds the numeric element to itself the {element} times foldly" $ do
      (sexpr, _, _) <- runCodeInstantly "(* 2 3 4)"
      sexpr @?= AtomInt 24
  , testCase "`/` gets the inverse value of the integral element foldly" $ do
      (sexpr, _, _) <- runCodeInstantly "(/ 4 2 2)"
      sexpr @?= AtomInt 1
  ]
