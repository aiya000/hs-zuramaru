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

test_preset_function :: [TestTree]
test_preset_function = defBang_test ++ letStar_test ++ call_test ++ addtional_test


-- def!
defBang_test :: [TestTree]
defBang_test =
  [ testCase "`def!` adds a value with a key to environment" $ do
      (sexpr, env, _) <- runCodeInstantly "(def! *poi* 10)"
      sexpr @?= AtomInt 10
      env ^? to (E.lookup "*poi*") . _Just
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


addtional_test :: [TestTree]
addtional_test =
  [ testCase "The lexical scope behavior is correct" $ do
      (result, env, _) <- runCodeInstantly "(let* (x 10) x)"
      -- the internal operation takes "x" well
      result @?= AtomInt 10
      -- "x" cannot be gotten in the outer scope
      E.lookup "x" env ^? _Just
        @?= Nothing
  ]
