{-# LANGUAGE OverloadedStrings #-}

module Maru.ParserTest where

import Data.Text (Text)
import Maru.Parser (parse, parseErrorPretty)
import Maru.Preprocessor (preprocess)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=), Assertion)
import qualified Maru.Type as MT

test_parser_and_preprocessor_shows :: [TestTree]
test_parser_and_preprocessor_shows =
  [ testCase "'123' as '123'" $ "123" `isShownAs` "123"
  , testCase "'abc' as 'abc'" $ "abc" `isShownAs` "abc"
  , testCase "'(123 456)' as '(123 456)'" $ "(123 456)" `isShownAs` "(123 456)"
  , testCase "'( 123 456 789 )' as '(123 456 789)'" $ "( 123 456 789 )" `isShownAs` "(123 456 789)"
  , testCase "'( + 2 (* 3 4) )' as '(+ 2 (* 3 4))'" $ "( + 2 (* 3 4) )" `isShownAs` "(+ 2 (* 3 4))"
  , testCase "'(1 )' as '(1)'" $ "(1 )" `isShownAs` "(1)"
  ]
  where
    isShownAs :: Text -> Text -> Assertion
    isShownAs code expected =
      case preprocess <$> parse code of
        Left  e     -> assertFailure $ "A parse is failed: " ++ parseErrorPretty e
        Right sexpr -> MT.readable sexpr @?= expected
