import Test.DocTest (doctest)

--TODO: These test may return the correctly return code constantly. Return the failure result if it is failed

main :: IO ()
main = do
  doctestIt "src/Maru/Type/SExpr.hs" ["src/Maru/Parser.hs"]
  doctestIt "src/Maru/Type/Eval.hs" []
  doctestIt "src/Maru/Eval.hs" []
  doctestIt "src/Maru/Eval/RuntimeOperation.hs" []


doctestIt :: FilePath -> [FilePath] -> IO ()
doctestIt path dependencies = do
  putStrLn $ path ++ ":"
  doctest $ dependencies ++ [path]
  putStrLn ""
