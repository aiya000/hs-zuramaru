{-# LANGUAGE OverloadedStrings #-}

module Steps.Step3Test where

import Control.Lens
import Maru.Type
import MaruTest (runCode, runCodeInstantly, runCodeWithSteps, StoppedPoint(..), prettyAssertFailure)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure, Assertion)
import qualified Data.Map.Lazy as M
import qualified Maru.Eval as E


-- initialEnv ∪ { (*x* : 10) }
modifiedEnv :: MaruEnv
modifiedEnv = M.insert "*x*" expectedTerm E.initialEnv
  where
    -- *x*: 10
    expectedTerm :: SomeMaruPrimitive
    expectedTerm = SomeMaruPrimitive DiscrSExpr $ AtomInt 10


test_preset_function :: [TestTree]
test_preset_function = set_test ++ find_test ++ get_test ++ addtional_naked_expression_test
  where
    set_test :: [TestTree]
    set_test =
      [ testCase "`set` adds a value with a key to the environment" $ do
          (sexpr, env, _) <- runCodeInstantly "(set *x* 10)"
          sexpr @?= AtomSymbol "*x*"
          env ^? to (M.lookup "*x*") . _Just . _SomeMaruPrimitive DiscrSExpr
                @?= Just (AtomInt 10)
      ]

    find_test :: [TestTree]
    find_test =
      [ testCase "`find` returns the value if the key is defined" $ do
          (sexpr, _, _) <- runCode modifiedEnv "(find *x*)"
          sexpr @?= AtomInt 10
      , testCase "`find` returns 'nil' if the key is not defined" $ do
          (sexpr, _, _) <- runCodeInstantly "(find this-is-an-undefined-variable)"
          sexpr @?= Nil
      ]

    get_test :: [TestTree]
    get_test =
      [ testCase "`get` returns the value if the key is defined" $ do
          (sexpr, _, _) <- runCode modifiedEnv "(get *x*)"
          sexpr @?= AtomInt 10
      , testCase "`get` throws an exception if the key is not defined" $
          expectEvalError "(get this-is-an-undefined-variable)"
      ]

    -- At step 3,
    -- These are the term
    --  10, 20, 1, 2
    --  x, *y*        -- if it is set
    --  (+ 10 20)     -- if this can be evaluated
    -- These are not the term but these are the expression
    --  (100), (undefined-func 10)  -- because this cannot be evaluated to the term
    addtional_naked_expression_test :: [TestTree]
    addtional_naked_expression_test =
      [ testCase "(1 2) to be the error (naked S expression)" $ expectEvalError "(1 2)"
      , testCase "(this-is-undefined-func 10) to be the error" $ expectEvalError "(this-is-undefined-func 10)"
      , testCase "10 not to be the error (naked term)" $ do
          (sexpr, _, _) <- runCodeInstantly "10"
          sexpr @?= AtomInt 10
      ]


expectEvalError :: SourceCode -> Assertion
expectEvalError code = do
      result <- runCodeWithSteps E.initialEnv code
      case result of
        EvalError  _   -> return ()
        ParseError msg -> assertFailure $ "The eval exception is expected, but the parse exception was thrown: " ++ msg
        Succeed sexpr env steps -> prettyAssertFailure sexpr env steps "The exception is expected, but got a some value"
