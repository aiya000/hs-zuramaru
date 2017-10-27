import Control.Exception.Safe (catch, SomeException)
import Control.Monad (when)
import Data.Monoid (All(..))
import Data.Profunctor (dimap)
import System.Exit (exitFailure)
import Test.DocTest (doctest)


main :: IO ()
main = do
  xs <- sequence [ doctestIt "src/Maru/Type/SExpr.hs" ["src/Maru/Parser.hs"]
                 , doctestIt "src/Maru/Type/Eval.hs" []
                 , doctestIt "src/Maru/Eval.hs" []
                 , doctestIt "src/Maru/Eval/RuntimeOperation.hs" []
                 ]
  when (dimap (map All) getAll mconcat xs) exitFailure


doctestIt :: FilePath -> [FilePath] -> IO Bool
doctestIt path dependencies = do
  putStrLn $ path ++ ":"
  x <- isExceptionThrown . doctest $ dependencies ++ [path]
  putStrLn ""
  return x
  where
    isExceptionThrown :: IO a -> IO Bool
    isExceptionThrown x = (x >> return False) `catch'` (return . const True)

    catch' :: IO Bool -> (SomeException -> IO Bool) -> IO Bool
    catch' = catch
