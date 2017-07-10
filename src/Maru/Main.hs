-- Suppress warnings what is happend by TemplateHaskell
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Maru.Main
  ( runRepl
  ) where

import Control.Eff (Eff, Member, SetMember, (:>))
import Control.Eff.Exception (throwExc, Fail, runFail, liftMaybe)
import Control.Eff.Lift (Lift, lift, runLift)
import Control.Eff.State.Lazy (State, runState)
import Control.Monad (mapM, when, void, forM_)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.State.Class (MonadState(..), gets)
import Data.Data (Data)
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Types.Injective (Injective(..))
import Data.Void (Void)
import Language.Haskell.TH (Name, mkName, nameBase, DecsQ)
import Lens.Micro ((.~))
import Lens.Micro.Mtl ((.=), (%=))
import Lens.Micro.TH (DefName(..), lensField, makeLensesFor, makeLensesWith, lensRules)
import Maru.Eval (MaruEnv)
import Maru.Type (SExpr, ParseErrorResult)
import System.Console.CmdArgs (cmdArgs, summary, program, help, name, explicit, (&=))
import TextShow (showt)
import qualified Control.Eff.State.Lazy as EST
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

--TODO: Use makeLensesA after GHC is fixed (https://ghc.haskell.org/trac/ghc/ticket/13932)
makeLensesFor [ ("debugMode", "debugModeA")
              , ("doEval", "doEvalA")
              ] ''CliOptions
--makeLensesA ''CliOptions

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

makeLensesFor [ ("readLogs", "readLogsA")
              , ("evalLogs", "evalLogsA")
              ] ''DebugLogs
--makeLensesA ''DebugLogs

emptyDebugLog :: DebugLogs
emptyDebugLog = DebugLogs [] []


-- | Integrate any type as @State@ of REPL.
data ReplState = ReplState
  { replOpts :: CliOptions -- ^ specified CLI options (not an initial value)
  , replEnv  :: MaruEnv    -- ^ The symbols of zuramaru
  , replLogs :: DebugLogs  -- ^ this value is appended in the runtime
  }

makeLensesFor [ ("replOpts", "replOptsA")
              , ("replEnv", "replEnvA")
              , ("replLogs", "replLogsA")
              ] ''ReplState
--makeLensesA ''ReplState


-- | For Lens Accessors
instance Member (State ReplState) r => MonadState ReplState (Eff r) where
  get = EST.get
  put = EST.put


-- |
-- The locally @MonadFail@ context
-- (This overrides existed @MonadFail@ instance)
instance Member Fail r => MonadFail (Eff r) where
  fail _ = throwExc ()

-- | Why @liftMaybeM@ is not defined in @Control.Eff.Exception@ ?
liftMaybeM :: ( Typeable m
              , Member Fail r, SetMember Lift (Lift m) r
              ) => m (Maybe a) -> Eff r a
liftMaybeM m = lift m >>= liftMaybe


instance Injective (Maybe ()) Bool where
  to (Just ()) = True
  to Nothing   = False


type EvalResult = Either ParseErrorResult SExpr
type Evaluator = MaruEnv -> SExpr -> IO (SExpr, MaruEnv)



-- | Run REPL of zuramaru
runRepl :: IO ()
runRepl = do
  options <- cmdArgs cliOptions
  let initialState = ReplState options Eval.initialEnv emptyDebugLog
  void . runLift $ runState initialState repl

--TODO: Use polymorphic type "(Member (State ReplState) r, SetMember Lift (Lift IO) r) => Eff r ()"
-- |
-- Do 'Loop' of 'Read', 'eval', and 'Print',
-- with the startup options.
--
-- If some command line arguments are given, enable debug mode.
-- Debug mode shows the parse and the evaluation's optionally result.
repl :: Eff (State ReplState :> Lift IO :> Void) ()
repl = do
  loopIsRequired <- to <$> runFail (rep :: Eff (Fail :> State ReplState :> Lift IO :> Void) ())
  when loopIsRequired repl

-- |
-- Do 'Read', 'Eval', and 'Print' of 'REPL'.
-- Return False if Ctrl+d is input.
-- Return True otherwise.
--
-- If @rep@ throws a () of the error, it means what the loop of REP exiting is required.
rep :: (Member Fail r, Member (State ReplState) r, SetMember Lift (Lift IO) r) => Eff r ()
rep = do
  input      <- liftMaybeM readPhase
  evalResult <- evalPhase input
  printPhase evalResult


-- |
-- Read line from stdin.
-- If stdin gives to interrupt, return Nothing.
-- If it's not, return it and it is added to history file
readPhase :: IO (Maybe Text)
readPhase = do
  maybeInput <- R.readline "zuramaru> "
  mapM R.addHistory maybeInput
  return (T.pack <$> maybeInput)

-- |
-- Do parse and evaluate a Text to a SExpr, and return its result.
-- The result is SEpxr (maru's AST) if a parse is succeed,
-- it is ParseErrorResult if the parse is failed.
--
-- Logs with @ParseResult@ if @Bool@ is True.
--
-- Execute the evaluation.
-- A state of @DebugLogs@ is updated by got logs which can be gotten in the evaluation.
-- A state of @MaruEnv@ is updated by new environment of the result.
evalPhase :: (Member (State ReplState) r, SetMember Lift (Lift IO) r) =>
             Text -> Eff r EvalResult
evalPhase code = do
  evalIsNeeded <- gets $ doEval . replOpts
  -- Get a real evaluator or an empty evaluator.
  -- The empty evaluator doesn't touch any arguments.
  let eval' = if evalIsNeeded then Eval.eval
                              else (return .) . flip (,)
  case Parser.debugParse code of
    (Left parseErrorResult, _) ->
      return $ Left parseErrorResult
    (Right sexpr, logs) -> do
      let (messages, item) = Parser.prettyShowLogs logs
      replLogsA . evalLogsA %= (++ messages ++ [item, "parse result: " <> showt sexpr]) --TODO: Replace to low order algorithm
      env              <- gets replEnv
      (result, newEnv) <- lift $ eval' env sexpr
      replEnvA .= newEnv
      return $ Right result

-- | Do 'Print' for a result of 'Read' and 'Eval'
printPhase :: (Member (State ReplState) r, SetMember Lift (Lift IO) r) =>
              EvalResult -> Eff r ()
printPhase sexprOrError = do
  DebugLogs readLogs' evalLogs' <- gets replLogs
  debugMode'                    <- gets $ debugMode . replOpts
  lift $ case sexprOrError of
    Left errorResult -> TIO.putStrLn . T.pack $ Parser.parseErrorPretty errorResult --TODO: Optimize error column and representation
    Right sexpr      -> TIO.putStrLn $ MT.visualize sexpr
  lift . when debugMode' $ do
    forM_ readLogs' $ TIO.putStrLn . ("<debug>(readPhase): " <>)
    forM_ evalLogs' $ TIO.putStrLn . ("<debug>(evalPhase): " <>)


-- |
-- makeLenses with 'A' suffix.
-- e.g. replEnv -> replEnvA
makeLensesA :: Name -> DecsQ
makeLensesA = makeLensesWith (lensRules & lensField .~ addSuffix)
  where
    addSuffix :: Name -> [Name] -> Name -> [DefName]
    addSuffix _ _ recordName = [TopName . mkName $ nameBase recordName ++ "A"]
