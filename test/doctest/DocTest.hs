import Test.DocTest (doctest)

--TODO: Find all .hs in ./src, and doctest it

main :: IO ()
main = do
  doctestIt "src" "src/Maru/Type/SExpr.hs"
    [ "src/Maru/Parser.hs"
    , "src/Maru/Preprocessor.hs"
    , "src/Maru/Eval.hs"
    , "src/Maru/Type/Eval.hs"
    , "src/Maru/Eval/RuntimeOperation.hs"
    ]

doctestIt :: FilePath -> FilePath -> [FilePath] -> IO ()
doctestIt baseDir path dependencies = do
  let nico = "-i" ++ baseDir
  doctest $ (nico:dependencies) ++ [path]
