{-# LANGUAGE OverloadedStrings #-}

module Steps.Step3_2Test where

import Control.Lens
import Maru.Type (SExpr(..), MaruEnv, SomeMaruPrimitive(..), _SomeMaruPrimitive, Discriminating(..))
import MaruTest (runCodeInstantly, runCode)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Data.Map.Lazy as M
import qualified Maru.Eval as E

test_preset_function :: [TestTree]
test_preset_function = defBang_test ++ letStar_test ++ call_test ++ addtional_test


-- def!
defBang_test :: [TestTree]
defBang_test =
  [ testCase "`def!` adds a value with a key to environment" $ do
      (sexpr, env, _) <- runCodeInstantly "(def! *x* 10)"
      sexpr @?= AtomInt 10
      env ^? to (M.lookup "*x*") . _Just . _SomeMaruPrimitive DiscrSExpr
        @?= Just (AtomInt 10)
  ]


-- let*
letStar_test :: [TestTree]
letStar_test =
  [ testCase "`let*` adds a value with akey to new environment scope" $ do
      (sexpr, _, _) <- runCodeInstantly "(let* (x 10) x)"
      sexpr @?= AtomInt 10
  ]


-- e.g. (+ 1 2), *y* to be called by `call`
-- (regard that *y* is set)
call_test :: [TestTree]
call_test =
  [ testCase "`call` calls a first element of the list as a function/macro with tail elements implicitly" $ do
      (sexpr, _, _) <- runCodeInstantly "(+ 1 2)"
      sexpr @?= AtomInt 3
      (sexpr, _, _) <- runCode modifiedEnv "*y*"
      sexpr @?= AtomInt 10
  ]
  where
    -- initialEnv ∪ { (*y* : 10) }
    modifiedEnv :: MaruEnv
    modifiedEnv = M.insert "*y*" expectedTerm E.initialEnv
    -- *x*: 10
    expectedTerm :: SomeMaruPrimitive
    expectedTerm = SomeMaruPrimitive DiscrSExpr $ AtomInt 10


addtional_test :: [TestTree]
addtional_test =
  [ testCase "After `let*` scope is end, a created variable is not existed" $ do
      (_, env, _) <- runCodeInstantly "(let* (x 10) x)"
      M.lookup "x" env ^? _Just . _SomeMaruPrimitive DiscrSExpr @?= Nothing
  ]
