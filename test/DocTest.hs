module DocTest where

import Test.DocTest (doctest)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

test_doctest :: [TestTree]
test_doctest =
  [ testCase "Maru.Type.SExpr" $ do
      putStrLn ""
      doctest ["src/Maru/Type/SExpr.hs"]
  ]
