{-# LANGUAGE OverloadedStrings #-}

module Steps.Step4Test where

import Data.Semigroup ((<>))
import Data.Text (Text)
import Maru.Type (readable, MaruSymbol(..), MaruEnv, SExpr(..))
import MaruTest (runCodeInstantly, runCode)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import qualified Data.Text as T


-- | 'printable' can be shown as 'printed'
itShouldBePrintable :: Text -> Text -> Assertion
itShouldBePrintable printable printed = do
  (sexpr, _, _) <- runCodeInstantly printable
  readable sexpr @?= printed


test_boolean_literals :: [TestTree]
test_boolean_literals =
  [ testCase "`true` literal is printable" $ do
      itShouldBePrintable "true" "true"
      itShouldBePrintable "(def! x true)" "true"
  , testCase "`false` literal is printable" $ do
      itShouldBePrintable "false" "false"
      itShouldBePrintable "(def! x false)" "false"
  ]


-- | My additional test
test_integral_negative_literals :: [TestTree]
test_integral_negative_literals =
  [ testCase "can be evaluated" $ do
      itShouldBePrintable "-1" "-1"
      itShouldBePrintable "(+ -1 2)" "1"
      itShouldBePrintable "(+ 2 -1)" "1"
  ]


test_do_macro :: [TestTree]
test_do_macro =
  [ testCase "evaluates taken arguments" $ do
      (sexpr, env, _) <- runCodeInstantly $ "(do (def! x 10)" <>
                                            "    (def! y (+ x 1))" <>
                                            "    (def! z (+ y 1)))"
      sexpr @?= AtomInt 12
      "x" `existedIn` env
      "y" `existedIn` env
      "z" `existedIn` env
  ]
  where
    -- She lives in the world
    existedIn :: MaruSymbol -> MaruEnv -> Assertion
    she `existedIn` world = do
      -- She undressed her dress.
      -- Her bareness was so mysterious...
      let nakedGoddess = unMaruSymbol she
      -- Also, her child was birthed.
      runCode world nakedGoddess
      -- She, her child, and all the families are so happy !
      return ()
      --NOTE: Don't afraid to delete these comment if these are obstacle :P
