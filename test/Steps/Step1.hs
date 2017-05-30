{-# LANGUAGE OverloadedStrings #-}

-- | 'Make a Lisp' requires
module Steps.Step1 where

import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=), Assertion)
import qualified Maru.Parser as Parser
import qualified Maru.Type as MT


test :: TestTree
test =
  testGroup "step1 requires the convertion of 'syntax -> syntax'"
    [ testCase "123 -> 123" $ "123" `isConvertedTo` "123"
    , testCase "abc -> abc" $ "abc" `isConvertedTo` "abc"
    , testCase "(123 456) -> (123 456)" $ "(123 456)" `isConvertedTo` "(123 456)"
    , testCase "( 123 456 789 ) -> (123 456 789)" $ "( 123 456 789 )" `isConvertedTo` "(123 456 789)"
    , testCase "( + 2 (* 3 4) ) -> (+ 2 (* 3 4))" $ "( + 2 (* 3 4) )" `isConvertedTo` "(+ 2 (* 3 4))"
    ]


isConvertedTo :: Text -> Text -> Assertion
isConvertedTo code expected =
  case Parser.parse code of
    Left  e     -> assertFailure $ "The parse is failed: " ++ Parser.parseErrorPretty e
    Right sexpr -> MT.toSyntax sexpr @?= expected
