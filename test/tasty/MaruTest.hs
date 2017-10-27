{-# LANGUAGE OverloadedStrings #-}

-- | The functions for help tests
module MaruTest
  ( StoppedPoint (..)
  , runCodeInstantly
  , runCode
  , runCodes
  , runCodeWithSteps
  , prettyAssertFailure
  , shouldBeEvaluatedTo
  , shouldBeEvaluatedTo'
  , isExistedIn
  , isNotExistedIn
  , runCorretly
  , runCodesCorrectly
  ) where

import Control.Lens (_1, _2, view, to, (<&>))
import Control.Monad (void, (>=>))
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Maru.Type
import System.IO.Silently (silence)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))
import qualified Data.Text.IO as TIO
import qualified Maru.Eval as E
import qualified Maru.Parser as P
import qualified Maru.Preprocessor as Pr

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
-- Similar to 'runCode', but multi expressions are taken.
-- And the previous result is continued.
runCodes :: MaruEnv -> NonEmpty SourceCode -> IO (SExpr, MaruEnv, SimplificationSteps)
runCodes env (code:|codes) = do
  let runners = flip map codes $ \x -> flip runCode x . view _2
  let tailExecutor = foldl' (>=>) (flip runCode code . view _2) runners
  runCode env "()" >>= tailExecutor

-- |
-- Run maru's source code in the specified `MaruEnv`.
--
-- If the parsing is failed, return `ParseError` .
-- If the evaluation is failed, return `EvalError`.
-- If all procedure is succeed, return `Succeed`.
runCodeWithSteps :: MaruEnv -> SourceCode -> IO StoppedPoint
runCodeWithSteps env code = do
  result <- mapM (E.eval env . Pr.preprocess) $ P.parse code
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


-- | 'code' can be evaluated to 'expected'
shouldBeEvaluatedTo :: SourceCode -> SourceCode -> Assertion
shouldBeEvaluatedTo code expected = do
  (sexpr, _, _) <- runCodeInstantly code
  readable sexpr @?= expected

-- | Similar to 'shouldBeEvaluatedTo' (comparison) and 'runCodes' (multi codes)
shouldBeEvaluatedTo' :: NonEmpty SourceCode -> SourceCode -> Assertion
shouldBeEvaluatedTo' codes expected = do
  actual <- silence $ runCodes E.initialEnv codes <&> view (_1 . to readable)
  actual @?= expected


-- | 'var' is existed in 'env'
isExistedIn :: MaruSymbol -> MaruEnv -> Assertion
var `isExistedIn` env = void . runCode env $ unMaruSymbol var

-- | 'var' is not existed in 'env'
isNotExistedIn :: MaruSymbol -> MaruEnv -> Assertion
var `isNotExistedIn` env = do
  point <- runCodeWithSteps env $ unMaruSymbol var
  case point of
    EvalError _ -> return ()
    x           -> assertFailure $ show x


-- | Mean that it returns something without the result
runCorretly :: Text -> Assertion
runCorretly = void . runCodeInstantly

-- | Similar to 'runCorretly' and 'shouldBeEvaluatedTo'`
runCodesCorrectly :: NonEmpty Text -> Assertion
runCodesCorrectly = void . runCodes E.initialEnv
