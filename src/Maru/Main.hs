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
import Control.Lens ((.~), (.=), (%=), DefName(..), lensField, makeLensesFor, makeLensesWith, lensRules)
import Control.Monad (mapM, when, void, forM_)
import Control.Monad.State.Class (MonadState(..), gets)
import Data.Data (Data)
import Data.Extensible
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Types.Injective (Injective(..))
import Language.Haskell.TH (Name, mkName, nameBase, DecsQ)
import Maru.Type
import Safe (tailMay)
import System.Console.CmdArgs (cmdArgs, summary, program, help, name, explicit, (&=))
import TextShow (showt)
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
instance Associate "stateRepl" (State ReplState) xs => MonadState ReplState (Eff xs) where
  get = getEff #stateRepl
  put = putEff #stateRepl


instance Injective (Maybe ()) Bool where
  to (Just ()) = True
  to Nothing   = False


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
  let initialState = ReplState options Eval.initialEnv emptyDebugLog
  void . retractEff @ "io" $ runStateEff @ "stateRepl" repl initialState

-- |
-- Do 'Loop' of 'Read', 'eval', and 'Print',
-- with the startup options.
--
-- If some command line arguments are given, enable debug mode.
-- Debug mode shows the parse and the evaluation's optionally result.
repl :: Eff [ "stateRepl" >: State ReplState
            , "io" >: IO
            ] ()
repl = do
  loopIsRequired <- to <$> runMaybeEff @ "maybe" rep
  when loopIsRequired repl

-- |
-- Do 'Read', 'Eval', and 'Print' of 'REPL'.
-- Return False if Ctrl+d is input.
-- Return True otherwise.
--
-- If @rep@ throws a () of the error, it means what the loop of REP exiting is required.
rep :: Eff '[ "maybe" >: MaybeEff
            , "stateRepl" >: State ReplState
            , "io" >: IO
            ] ()
rep = do
  input      <- castEff readPhase
  evalResult <- castEff $ evalPhase input
  printPhase evalResult


-- |
-- Read line from stdin.
-- If stdin gives to interrupt, return Nothing.
-- If it's not, return it and it is added to history file
readPhase :: Eff '[ "maybe" >: MaybeEff
                  , "io" >: IO
                  ] Text
readPhase = do
  maybeInput <- liftEff #io $ R.readline "zuramaru> "
  liftEff #io $ mapM R.addHistory maybeInput
  T.pack <$> liftMaybe maybeInput

--TODO: why?
--liftMaybe :: Associate k MaybeEff xs => Proxy k -> Maybe a -> Eff xs a
--liftMaybe k Nothing  = throwEff k ()
--liftMaybe _ (Just x) = return x
liftMaybe :: Maybe a -> Eff '["maybe" >: MaybeEff, "io" >: IO] a
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
evalPhase :: Text -> Eff '[ "stateRepl" >: State ReplState
                          , "io" >: IO
                          ] EvalPhaseResult
evalPhase code = do
  evalIsNeeded <- gets $ doEval . replOpts
  -- Get a real evaluator or an empty evaluator.
  -- The empty evaluator doesn't touch any arguments.
  let eval' = if evalIsNeeded then Eval.eval
                              else fakeEval
  case Parser.debugParse code of
    (Left parseErrorResult, _) -> return $ ParseError parseErrorResult
    (Right sexpr, logs) -> do
      let logs'  = map unParseLog logs
          newLog = "parse result: " <> showt sexpr
      replLogsA . evalLogsA %= (++ newLog : logs')
      env        <- gets replEnv
      evalResult <- liftEff #io $ eval' env sexpr
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
              , Associate "io" IO xs
              ) => EvalPhaseResult -> Eff xs ()
printPhase result = do
  DebugLogs readLogs' evalLogs' <- gets replLogs
  debugMode'                    <- gets $ debugMode . replOpts
  liftEff #io $ case result of
    ParseError e      -> TIO.putStrLn . T.pack . forgetMatrixAnnotation $ Parser.parseErrorPretty e
    EvalError  e      -> TIO.putStrLn . T.pack $ show e
    RightResult sexpr -> TIO.putStrLn $ MT.visualize sexpr
  liftEff #io . when debugMode' $ do
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
-- makeLenses with 'A' suffix.
-- e.g. replEnv -> replEnvA
makeLensesA :: Name -> DecsQ
makeLensesA = makeLensesWith (lensRules & lensField .~ addSuffix)
  where
    addSuffix :: Name -> [Name] -> Name -> [DefName]
    addSuffix _ _ recordName = [TopName . mkName $ nameBase recordName ++ "A"]
