module DocTest where

import Test.DocTest (doctest)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

test_doctest :: [TestTree]
test_doctest =
  [ doctestCase "src/Maru/Type/SExpr.hs" ["src/Maru/Parser.hs"]
  , doctestCase "src/Maru/Type/Eval.hs" []
  , doctestCase "src/Maru/Eval.hs" []
  , doctestCase "src/Maru/Eval/RuntimeOperation.hs" []
  ]


doctestCase :: FilePath -> [FilePath] -> TestTree
doctestCase path dependencies =
  testCase path $ do
    putStrLn ""
    doctest $ dependencies ++ [path]
