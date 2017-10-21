-- Suppress warnings what is happend by TemplateHaskell
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Maru.Main
  ( runRepl
  ) where

import Control.Exception.Safe (SomeException)
import Control.Lens (view, (%=), (.=), Iso', iso)
import Control.Monad (mapM, when, void, forM_)
import Control.Monad.State.Class (MonadState(..), gets)
import Data.Data (Data)
import Data.Extensible
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Maru.TH (makeLensesA)
import Maru.Type
import Safe (tailMay)
import System.Console.CmdArgs (cmdArgs, summary, program, help, name, explicit, (&=))
import TextShow (showt)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Maru.Eval as E
import qualified Maru.Parser as Parser
import qualified Maru.Type as MT
import qualified System.Console.Readline as R

-- | Command line options
data CliOptions = CliOptions
  { debugMode :: Bool
  , doEval    :: Bool
  } deriving (Show, Data, Typeable)

makeLensesA ''CliOptions

-- | Default of `CliOptions`
cliOptions :: CliOptions
cliOptions = CliOptions
  { debugMode = False &= name "debug"
  , doEval    = True &= name "do-eval"
                     &= help "If you don't want to evaluation, disable this"
                     &= explicit
  }
  &= summary "マルのLisp処理系ずら〜〜"
  &= program "maru"


-- |
-- Logs of REPL.
--
-- This is collected in 'Read' and 'Eval' phase of REPL,
-- and this is shown in 'Print' phase of REPL.
--
-- This is not shown if you doesn't specifiy --debug.
data DebugLogs = DebugLogs
  { readLogs :: [Text]
  , evalLogs :: [Text]
  } deriving (Show)

makeLensesA ''DebugLogs

-- | An empty value of `DebugLogs`
emptyDebugLog :: DebugLogs
emptyDebugLog = DebugLogs [] []


-- | Integrate any type as @State@ of REPL.
data ReplState = ReplState
  { replOpts :: CliOptions -- ^ specified CLI options (not an initial value)
  , replEnv  :: MaruEnv    -- ^ The symbols of zuramaru
  , replLogs :: DebugLogs  -- ^ this value is appended in the runtime
  }

makeLensesA ''ReplState


-- | For Lens Accessors
instance Associate "stateRepl" (State ReplState) xs => MonadState ReplState (Eff xs) where
  get = getEff #stateRepl
  put = putEff #stateRepl


-- |
-- The eval phase do parse and evaluation,
-- take its error or a rightly result
data EvalPhaseResult = ParseError ParseErrorResult -- ^ An error is happened in the parse
                     | EvalError SomeException     -- ^ An error is happend in the evaluation
                     | RightResult SExpr           -- ^ A result is made by the parse and the evaulation without errors

type Evaluator = MaruEnv -> SExpr -> IO (Either SomeException (SExpr, MaruEnv, SimplificationSteps))


-- | Run REPL of zuramaru
runRepl :: IO ()
runRepl = do
  options <- cmdArgs cliOptions
  let initialState = ReplState options E.initialEnv emptyDebugLog
  void . retractEff @ IOEffKey $ runStateEff @ "stateRepl" repl initialState

-- |
-- Do 'Loop' of 'Read', 'eval', and 'Print',
-- with the startup options.
--
-- The state of `ReplState` is initialized before the one of "READ-EVAL-PRINT" of the "LOOP" is ran.
--
-- If some command line arguments are given, enable debug mode.
-- Debug mode shows the parse and the evaluation's optionally result.
repl :: Eff '["stateRepl" >: State ReplState, IOEff] ()
repl = do
  loopIsRequired <- view _iso <$> runMaybeEff @ "maybe" rep
  modifyEff #stateRepl $ \x -> x { replLogs = emptyDebugLog }
  when loopIsRequired repl

-- |
-- Do 'Read', 'Eval', and 'Print' of 'REPL'.
-- Return False if Ctrl+d is input.
-- Return True otherwise.
--
-- If @rep@ throws a () of the error, it means what the loop of REP exiting is required.
rep :: Eff '[ "maybe" >: MaybeEff
            , "stateRepl" >: State ReplState
            , IOEff
            ] ()
rep = do
  input      <- castEff readPhase
  evalResult <- castEff $ evalPhase input
  printPhase evalResult


-- |
-- Read line from stdin.
-- If stdin gives to interrupt, return Nothing.
-- If it's not, return it and it is added to history file
readPhase :: Eff '["maybe" >: MaybeEff, IOEff] Text
readPhase = do
  maybeInput <- liftIOEff $ R.readline "zuramaru> "
  liftIOEff $ mapM R.addHistory maybeInput
  T.pack <$> liftMaybe maybeInput

-- | Lift up `Nothing` to the failure of the whole
liftMaybe :: Associate "maybe" MaybeEff xs => Maybe a -> Eff xs a
liftMaybe Nothing  = throwEff #maybe ()
liftMaybe (Just x) = return x


-- |
-- Do parse and evaluate a Text to a SExpr.
-- Return @SExpr@ (maru's AST) if both parse and evaluation is succeed.
-- Otherwise, return a error result.
--
-- Execute the evaluation.
-- A state of @DebugLogs@ is updated by got logs which can be gotten in the evaluation.
-- A state of @MaruEnv@ is updated by new environment of the result.
evalPhase :: Text -> Eff '["stateRepl" >: State ReplState, IOEff] EvalPhaseResult
evalPhase code = do
  evalIsNeeded <- gets $ doEval . replOpts
  -- Get a real evaluator or an empty evaluator.
  -- The empty evaluator doesn't touch any arguments.
  let eval' = if evalIsNeeded then E.eval
                              else fakeEval
  case Parser.debugParse code of
    (Left parseErrorResult, _) -> return $ ParseError parseErrorResult
    (Right sexpr, logs) -> do
      let logs'  = map unParseLog logs
          newLog = "parse result: " <> showt sexpr
      replLogsA . evalLogsA %= (++ newLog : logs')
      env        <- gets replEnv
      evalResult <- liftIOEff $ eval' env sexpr
      case evalResult of
        Left evalErrorResult -> return $ EvalError evalErrorResult
        Right (result, newEnv, steps) -> do
          replEnvA .= newEnv
          replLogsA . evalLogsA %= (++ reportSteps steps)
          return $ RightResult result
  where
    -- Do nothing
    fakeEval :: Evaluator
    fakeEval = (return .) . (Right .) . flip (,,[])


-- | Do 'Print' for a result of 'Read' and 'Eval'
printPhase :: ( Associate "stateRepl" (State ReplState) xs
              , IOEffAssociation xs
              ) => EvalPhaseResult -> Eff xs ()
printPhase result = do
  DebugLogs readLogs' evalLogs' <- gets replLogs
  debugMode'                    <- gets $ debugMode . replOpts
  liftIOEff $ case result of
    ParseError e      -> TIO.putStrLn . T.pack . forgetMatrixAnnotation $ Parser.parseErrorPretty e
    EvalError  e      -> TIO.putStrLn . T.pack $ show e
    RightResult sexpr -> TIO.putStrLn $ MT.readable sexpr
  liftIOEff . when debugMode' $ do
    forM_ readLogs' $ TIO.putStrLn . ("<debug>(readPhase): " <>)
    forM_ evalLogs' $ TIO.putStrLn . ("<debug>(evalPhase): " <>)
  where
    --NOTE: A result of Megaparse's `parseErrorPretty` may have about column and row of an error
    forgetMatrixAnnotation :: String -> String
    forgetMatrixAnnotation parseErrorInfo =
      case tailMay $ lines parseErrorInfo of
        Nothing -> ""
        Just xs  -> unlines xs


-- |
-- An isomorphism of between `Maybe ()` and `Bool`.
-- `Just ()` is mapped to `True`
_iso :: Iso' (Maybe ()) Bool
_iso = iso to from
  where
    to :: Maybe () -> Bool
    to (Just ()) = True
    to Nothing   = False
    from :: Bool -> Maybe ()
    from True  = Just ()
    from False = Nothing
