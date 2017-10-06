-- | The functions for help tests
module MaruTest
  ( StoppedPoint (..)
  , runCodeInstantly
  , runCode
  , runCodeWithSteps
  , prettyAssertFailure
  ) where

import Maru.Type (SExpr(..), MaruEnv, SimplificationSteps, SourceCode, reportSteps)
import Test.Tasty.HUnit (Assertion, assertFailure)
import qualified Data.Text.IO as TIO
import qualified Maru.Eval as E
import qualified Maru.Parser as P

-- |
-- `ParseError` gives a cause of the parse error.
-- `EvalError` is same as `ParseError` but for the evaluation.
-- `Succeed` gives the results of the execution.
-- ("execution" means "parse" + "evaluate")
--
-- Please see `runCodeWithSteps` for more information.
data StoppedPoint = ParseError String
                  | EvalError String
                  | Succeed SExpr MaruEnv SimplificationSteps
  deriving (Show)


-- |
-- Similar to `runCode`.
-- Run maru's source code in new `MaruEnv`,
runCodeInstantly :: SourceCode -> IO (SExpr, MaruEnv, SimplificationSteps)
runCodeInstantly = runCode E.initialEnv

-- |
-- Similar to `runCodeWithSteps`,
-- but `ParseError` and `EvalError` are thrown as the context of `IO`
runCode :: MaruEnv -> SourceCode -> IO (SExpr, MaruEnv, SimplificationSteps)
runCode env code = do
  result <- runCodeWithSteps env code
  case result of
    EvalError  msg -> fail msg
    ParseError msg -> fail msg
    Succeed x y z -> return (x, y, z)

-- |
-- Run maru's source code in the specified `MaruEnv`.
--
-- If the parsing is failed, return `ParseError` .
-- If the evaluation is failed, return `EvalError`.
-- If all procedure is succeed, return `Succeed`.
runCodeWithSteps :: MaruEnv -> SourceCode -> IO StoppedPoint
runCodeWithSteps env code = do
  result <- mapM (E.eval env) $ P.parse code
  case result of
    Left e                  -> return . ParseError $ show e
    Right (Left e)          -> return . EvalError $ show e
    Right (Right (x, y, z)) -> return $ Succeed x y z

-- |
-- Pretty print the results,
-- and Make the failure this `Assertion`
prettyAssertFailure :: SExpr -> MaruEnv -> SimplificationSteps -> String -> Assertion
prettyAssertFailure sexpr env steps msg = do
  putStrLn "Fail"
  putStrLn msg
  putStrLn $ "got output result: " ++ show sexpr
  putStrLn $ "got final environment: " ++ show env
  putStrLn "got logs:"
  mapM_ TIO.putStrLn $ reportSteps steps
  assertFailure ""
