module Maru.Type.EvalTest where

import Control.Monad.Fail (fail)
import Maru.Type.Eval (Result(..))
import Prelude hiding (fail)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Data.Text as T


oops :: String
oops = "Oops xD"


test_result_type_throws_the_exception_as_a_pure_value_by_fail :: [TestTree]
test_result_type_throws_the_exception_as_a_pure_value_by_fail =
  [ testCase "" $ negativeContext oops @?= fail' oops
  ]
  where
    fail' :: String -> Result ()
    fail' = Result . Left . T.pack


-- | An alias of `fail`
negativeContext :: String -> Result ()
negativeContext = fail
