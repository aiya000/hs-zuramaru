{-# LANGUAGE OverloadedStrings #-}

module Steps.Step4Test where

import Data.Text (Text)
import Maru.Type (readable)
import MaruTest (runCodeInstantly)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)

test_boolean_literals :: [TestTree]
test_boolean_literals =
  [ testCase "`true` literal is printable" $ do
      itShouldBePrintable "true" "true"
      itShouldBePrintable "(def! x true)" "true"
  , testCase "`false` literal is printable" $ do
      itShouldBePrintable "false" "false"
      itShouldBePrintable "(def! x false)" "false"
  ]
  where
    -- 'printable' can be shown as 'printed'
    itShouldBePrintable :: Text -> Text -> Assertion
    itShouldBePrintable printable printed = do
      (sexpr, _, _) <- runCodeInstantly printable
      readable sexpr @?= printed
