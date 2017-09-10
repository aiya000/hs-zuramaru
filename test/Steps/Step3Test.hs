{-# LANGUAGE OverloadedStrings #-}

module Steps.Step3Test where

import Control.Lens
import Data.Text (Text)
import Maru.Type
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import qualified Data.Map.Lazy as M
import qualified Maru.Eval as E
import qualified Maru.Parser as P


test_preset_function :: [TestTree]
test_preset_function = set_test ++ find_test ++ get_test
  where
    set_test :: [TestTree]
    set_test =
      [ testCase "`set` adds a value with a key to the environment" $ do
          (sexpr, env) <- runCode "(set *x* 10)"
          sexpr @?= AtomSymbol "*x*"
          env ^? to (M.lookup "*x*") . _Just . _SomeMaruPrimitive DiscrInt
                @?= Just 10
      ]

    -- initialEnv ∪ { (*x* : 10) }
    modifiedEnv :: MaruEnv
    modifiedEnv = M.insert "*x*" expectedTerm E.initialEnv
    -- *x*: 10
    expectedTerm :: SomeMaruPrimitive
    expectedTerm = SomeMaruPrimitive DiscrInt 10

    find_test :: [TestTree]
    find_test =
      [ testCase "`find` returns the value if the key is defined" $ do
          (sexpr, _) <- runCode' modifiedEnv "(find *x*)"
          sexpr @?= AtomInt 10
      , testCase "`find` returns 'nil' if the key is not defined" $ do
          (sexpr, _) <- runCode "(find this-is-an-undefined-variable)"
          sexpr @?= Nil
      ]

    get_test :: [TestTree]
    get_test =
      [ testCase "`get` returns the value if the key is defined" $ do
          (sexpr, _) <- runCode' modifiedEnv "(get *x*)"
          sexpr @?= AtomInt 10
      , testCase "`get` throws an exception if the key is not defined" $ do
          result <- sequence $ E.eval E.initialEnv <$> P.parse "(find this-is-an-undefined-variable)"
          case result of
            -- the parse is succeed, but the evaluation is failed
            Right (Left _) -> return ()
            _              -> assertFailure "The exception is expected, but the exception was not thrown"
      ]


-- |
-- Run maru's source code in new `MaruEnv`,
-- Return the pair of a result value and a result env
runCode :: Text -> IO (SExpr, MaruEnv)
runCode = runCode' E.initialEnv

-- | Run maru's source code in the specified `MaruEnv`
runCode' :: MaruEnv -> Text -> IO (SExpr, MaruEnv)
runCode' env code = do
  result <- sequence $ E.eval env <$> P.parse code
  case result of
    Left e                  -> fail $ show e
    Right (Left e)          -> fail $ show e
    Right (Right (x, y, _)) -> return (x, y)
