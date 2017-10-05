{-# LANGUAGE OverloadedStrings #-}

module Steps.Step4Test where

import Data.Semigroup ((<>))
import Data.Text (Text)
import Maru.Type (readable, MaruSymbol(..), MaruEnv, SExpr(..))
import MaruTest (runCodeInstantly, runCode)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import qualified Data.Text as T


-- | 'code' can be evaluated to 'expected'
shouldBeEvaluatedTo :: Text -> Text -> Assertion
shouldBeEvaluatedTo code expected = do
  (sexpr, _, _) <- runCodeInstantly code
  readable sexpr @?= expected


test_boolean_literals :: [TestTree]
test_boolean_literals =
  [ testCase "`true` literal is printable" $ do
      "true"          `shouldBeEvaluatedTo` "true"
      "(def! x true)" `shouldBeEvaluatedTo` "true"
  , testCase "`false` literal is printable" $ do
      "false"          `shouldBeEvaluatedTo` "false"
      "(def! x false)" `shouldBeEvaluatedTo` "false"
  ]


-- | My additional test
test_integral_negative_literals :: [TestTree]
test_integral_negative_literals =
  [ testCase "can be evaluated" $ do
      "-1"       `shouldBeEvaluatedTo` "-1"
      "(+ -1 2)" `shouldBeEvaluatedTo` "1"
      "(+ 2 -1)" `shouldBeEvaluatedTo` "1"
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
