import Test.DocTest (doctest)

main :: IO ()
main = do
  doctestIt "src/Maru/Type/SExpr.hs" ["src/Maru/Parser.hs"]
  doctestIt "src/Maru/Type/Eval.hs" []
  doctestIt "src/Maru/Eval.hs" []
  doctestIt "src/Maru/Eval/RuntimeOperation.hs" []


doctestIt :: FilePath -> [FilePath] -> IO ()
doctestIt path dependencies = doctest $ dependencies ++ [path]
