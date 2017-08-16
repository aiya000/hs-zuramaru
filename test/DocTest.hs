module DocTest where

import Test.DocTest (doctest)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

test_doctest :: [TestTree]
test_doctest =
  [ doctestCase "src/Maru/Type/SExpr.hs"
  , doctestCase "src/Maru/Type/Eff.hs"
  , doctestCase "src/Maru/Eval/RuntimeOperation.hs"
  ]

doctestCase :: FilePath -> TestTree
doctestCase path =
  testCase path $ do
    putStrLn ""
    doctest [path]
