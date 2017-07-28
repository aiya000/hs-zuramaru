module Maru.Type.EvalTest where

import Control.Monad.Fail (fail)
import Data.Either (isLeft)
import Maru.Eval (initialEnv)
import Maru.Type (MaruEvaluator, runMaruEvaluator)
import Prelude hiding (fail)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?))


test_MaruEvaluator_fail_is_pure :: [TestTree]
test_MaruEvaluator_fail_is_pure =
  [ testCase "fail is same as throwExc" $ do
      (result, _, _) <- runMaruEvaluator failureContext initialEnv
      isLeft result @? "This expression should be executed" -- This maybe executed if MaruEvaluator's fail is not impure
  , testCase "An another type's pattern match failure in MaruEvaluator context, it can be converted to MaruEvaluator's fail" $ do
      (result, _, _) <- runMaruEvaluator patternMatchFailContext initialEnv
      isLeft result @? "This expression should be executed"
  ]
  where
    failureContext :: MaruEvaluator ()
    failureContext = fail "failure in MaruEvaluator context"

    patternMatchFailContext :: MaruEvaluator ()
    patternMatchFailContext = do
      Just x <- return (Nothing :: Maybe ())
      x `seq` return ()

    --TODO: Define in somewhere or use something instead
    liftFail :: MaruEvaluator (Maybe ()) -> MaruEvaluator ()
    liftFail = undefined
