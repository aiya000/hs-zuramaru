{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Maru.Main
  ( runRepl
  ) where


import Control.Monad (mapM, when)
import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Text (Text)
import Maru.Eval (MaruEnv)
import Maru.Type (SExpr, ParseLog, ParseErrorResult)
import System.Console.CmdArgs (cmdArgs, summary, program, help, name, explicit, (&=), Data, Typeable)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Maru.Eval as Eval
import qualified Maru.Parser as Parser
import qualified Maru.Type as MT
import qualified System.Console.Readline as R

-- | Command line options
data CliOptions = CliOptions
  { debugMode :: Bool
  , doEval    :: Bool
  } deriving (Show, Data, Typeable)

-- | Default of @CliOptions@
cliOptions :: CliOptions
cliOptions = CliOptions
  { debugMode = False &= name "debug"
  , doEval    = True &= name "do-eval"
                     &= help "If you don't want to evaluation, disable this"
                     &= explicit
  }
  &= summary "マルのLisp処理系ずら〜〜"
  &= program "maru"


-- | @a@ is exist or nothing
data StandBy a b = With a b | Lonely b

instance Functor (StandBy a) where
  fmap f (a `With` b) = a `With` f b
  fmap f (Lonely x) = Lonely $ f x

instance Applicative (StandBy a) where
  pure = Lonely
  (Lonely f) <*> x = fmap f x
  (a `With` f) <*> (Lonely b) = a `With` f b
  (a `With` f) <*> (_ `With` b) = a `With` f b


type EvalResult = Either ParseErrorResult SExpr


-- | Run REPL of Maru
runRepl :: IO ()
runRepl = do
  options <- cmdArgs cliOptions
  run options
  where
    -- An argument is needed by the loop, it can be anything,
    -- with specified cli options.
    continue :: CliOptions -> () -> IO ()
    continue options () = flip runContT (continue options) . ContT $ repl options Eval.initialEnv
    -- An alias
    run = flip continue ()

-- |
-- Do 'Loop' of 'Read', 'eval', and 'Print',
-- with the startup options.
--
-- This signature of the type means a stuff of @ContT@.
--
-- If some command line arguments are given, enable debug mode.
-- Debug mode shows the parse and the evaluation's optionally result.
repl :: CliOptions -> MaruEnv -> (() -> IO ()) -> IO ()
repl options env continue = do
  loopIsRequired <- iso <$> runMaybeT (rep options env)
  when loopIsRequired $ continue ()
  where
    -- Do 'Read', 'Eval', and 'Print' of 'REPL'.
    -- Return False if Ctrl+d is input.
    -- Return True otherwise.
    --
    -- Branch by @CliOptions@.
    --
    -- If this result is Nothing, it means what the loop of REP exiting is required.
    rep :: CliOptions -> MaruEnv -> MaybeT IO ()
    rep options env = do
      input          <- MaybeT readPhase
      resultWithLogs <- lift $ evalPhase options env input
      lift $ printPhase resultWithLogs

    -- Read line from stdin.
    -- If stdin gives to interrupt, return Nothing.
    -- If it's not, return it and it is added to history file
    readPhase :: IO (Maybe Text)
    readPhase = do
      maybeInput <- R.readline "zuramaru> "
      mapM R.addHistory maybeInput
      return $ fmap T.pack maybeInput

    -- Do parse and evaluate a Text to a SExpr, and return its result.
    -- The result is SEpxr (maru's AST) if a parse is succeed,
    -- it is ParseErrorResult if the parse is failed.
    --
    -- Logs with @ParseResult@ if @Bool@ is True.
    evalPhase :: CliOptions -> MaruEnv -> Text -> IO ([ParseLog] `StandBy` EvalResult)
    evalPhase (CliOptions False evalIsNeeded) env code =
      case Parser.parse code of
        Left parseErrorResult -> return . Lonely $ Left parseErrorResult
        --TODO: Don't abandon new MaruEnv
        Right sexpr -> do
          let evalOrNOOP = getEvaluator evalIsNeeded
          Lonely . Right . fst <$> evalOrNOOP env sexpr

    evalPhase (CliOptions True evalIsNeeded) env code =
      case Parser.debugParse code of
        (Left parseErrorResult, _) -> return . Lonely $ Left parseErrorResult 
        (Right sexpr, logs) -> do
          let evalOrNOOP = getEvaluator evalIsNeeded
          With logs . Right . fst <$> evalOrNOOP env sexpr

    -- If --do-eval=False is specified,
    -- return a function that doesn't touch arguments.
    getEvaluator :: Bool -> MaruEnv -> SExpr -> IO (SExpr, MaruEnv)
    getEvaluator True  = Eval.eval
    getEvaluator False = (return .) . flip (,)

    -- Do 'Print' for a result of 'Read' and 'Eval'
    printPhase :: [ParseLog] `StandBy` EvalResult -> IO ()
    printPhase (Lonely sexprOrError) = printEvalResult sexprOrError

    -- And show logs of the parse
    printPhase (logs `With` sexprOrError) = do
      printEvalResult sexprOrError
      Parser.prettyPrintLogs logs

    printEvalResult :: EvalResult -> IO ()
    printEvalResult (Left errorResult) = tPutStrLn $ Parser.parseErrorPretty errorResult --TODO: Optimize error column and representation
    printEvalResult (Right sexpr)      = TIO.putStrLn $ MT.visualize sexpr


-- |
-- Regard String as Text.
-- And apply putStrLn to it
tPutStrLn :: String -> IO ()
tPutStrLn = TIO.putStrLn . T.pack


-- |
-- An isomorphism of Maybe () to Bool,
--
-- @Just ()@ is regarted to @True@.
-- @Nothing@ is regarted to @False@.
-- (Just () ~= True, Nothing ~= False.)
iso :: Maybe () -> Bool
iso (Just _) = True
iso Nothing  = False
