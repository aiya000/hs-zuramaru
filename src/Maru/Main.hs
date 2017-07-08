-- Suppress warnings what is happend by TemplateHaskell
{-# OPTION_GHC -Wno-unused-top-binds #-}

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

import Control.Eff (Eff, Member, SetMember)
import Control.Eff.Lift (Lift, lift, runLift)
import Control.Eff.State.Lazy (State, runState)
import Control.Monad (mapM, when, void, forM_)
import Control.Monad.State.Class (MonadState(..), gets)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Data (Data)
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Language.Haskell.TH (Name, mkName, nameBase, DecsQ, )
import Lens.Micro ((.~))
import Lens.Micro.Mtl ((.=), (%=))
import Lens.Micro.TH (DefName(..), lensField, makeLensesFor, makeLensesWith, lensRules)
import Maru.Eval (MaruEnv)
import Maru.Type (SExpr, ParseLog, ParseErrorResult)
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


---TODO: Use Text instead of String for an algorithm order and locales
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


type EvalResult = Either ParseErrorResult SExpr
type Evaluator = MaruEnv -> SExpr -> IO (SExpr, MaruEnv)


-- | For Lens Accessors
instance Member (State ReplState) r => MonadState ReplState (Eff r) where
  get = EST.get
  put = EST.put


-- | Run REPL of zuramaru
runRepl :: IO ()
runRepl = do
  options <- cmdArgs cliOptions
  let initialState = ReplState options Eval.initialEnv emptyDebugLog
  void . runLift $ runState initialState repl

--TODO: Organize scopes
-- |
-- Do 'Loop' of 'Read', 'eval', and 'Print',
-- with the startup options.
--
-- If some command line arguments are given, enable debug mode.
-- Debug mode shows the parse and the evaluation's optionally result.
repl :: (Member (State ReplState) r, SetMember Lift (Lift IO) r) => Eff r ()
repl = do
  loopIsRequired <- iso <$> rep
  when loopIsRequired repl
  where
    -- Do 'Read', 'Eval', and 'Print' of 'REPL'.
    -- Return False if Ctrl+d is input.
    -- Return True otherwise.
    --
    -- If this result is Nothing, it means what the loop of REP exiting is required.
    rep :: (Member (State ReplState) r, SetMember Lift (Lift IO) r) => Eff r (Maybe ())
    rep = do
      --TODO: Use a context of MaybeT (for fmap omission)
      maybeInput      <- lift readPhase
      maybeEvalResult <- sequence (evalPhase <$> maybeInput)
      sequence (printPhase <$> maybeEvalResult)

    -- Read line from stdin.
    -- If stdin gives to interrupt, return Nothing.
    -- If it's not, return it and it is added to history file
    readPhase :: IO (Maybe Text)
    readPhase = do
      maybeInput <- R.readline "zuramaru> "
      mapM R.addHistory maybeInput
      return (T.pack <$> maybeInput)

    -- Do parse and evaluate a Text to a SExpr, and return its result.
    -- The result is SEpxr (maru's AST) if a parse is succeed,
    -- it is ParseErrorResult if the parse is failed.
    --
    -- Logs with @ParseResult@ if @Bool@ is True.
    evalPhase :: (Member (State ReplState) r, SetMember Lift (Lift IO) r) =>
                 Text -> Eff r EvalResult
    evalPhase code = do
      opts <- gets replOpts
      let evalOrNOOP = getEvaluator $ doEval opts
      if debugMode opts
         then evalPhaseInDebugMode code evalOrNOOP
         else evalPhase' code evalOrNOOP

    evalPhase' :: (Member (State ReplState) r, SetMember Lift (Lift IO) r) =>
                 Text -> Evaluator -> Eff r EvalResult
    evalPhase' code eval = do
      env <- gets replEnv
      case Parser.parse code of
        Left parseErrorResult -> return $ Left parseErrorResult
        Right sexpr -> do
          --TODO: DRY (evalPhaseInDebugMode)
          (result, newEnv) <- lift $ eval env sexpr
          replEnvA .= newEnv
          return $ Right result

    evalPhaseInDebugMode :: (Member (State ReplState) r, SetMember Lift (Lift IO) r) =>
                 Text -> Evaluator -> Eff r EvalResult
    evalPhaseInDebugMode code eval = do
      env <- gets replEnv
      case Parser.debugParse code of
        (Left parseErrorResult, _) -> return $ Left parseErrorResult
        (Right sexpr, logs) -> do
          --TODO: DRY (evalPhase')
          let (messages, item) = Parser.prettyShowLogs logs
          replLogsA . evalLogsA %= (++ messages ++ item : ["parse result: " <> showt sexpr]) --TODO: Replace to low order algorithm
          (result, newEnv) <- lift $ eval env sexpr
          replEnvA .= newEnv
          return $ Right result

    -- If --do-eval=False is specified,
    -- return a function that doesn't touch arguments.
    getEvaluator :: Bool -> Evaluator
    getEvaluator True  = Eval.eval
    getEvaluator False = (return .) . flip (,)

    -- Do 'Print' for a result of 'Read' and 'Eval'
    printPhase :: (Member (State ReplState) r, SetMember Lift (Lift IO) r) =>
                  EvalResult -> Eff r ()
    printPhase sexprOrError = do
      DebugLogs readLogs' evalLogs' <- gets replLogs
      debugMode'                    <- gets $ debugMode . replOpts
      lift $ case sexprOrError of
        Left errorResult -> tPutStrLn $ Parser.parseErrorPretty errorResult --TODO: Optimize error column and representation
        Right sexpr      -> TIO.putStrLn $ MT.visualize sexpr
      lift . when debugMode' $ do
        forM_ readLogs' $ TIO.putStrLn . ("<debug>(readPhase): " <>)
        forM_ evalLogs' $ TIO.putStrLn . ("<debug>(evalPhase): " <>)


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


-- |
-- makeLenses with 'A' suffix.
-- e.g. replEnv -> replEnvA
makeLensesA :: Name -> DecsQ
makeLensesA = makeLensesWith (lensRules & lensField .~ addSuffix)
  where
    addSuffix :: Name -> [Name] -> Name -> [DefName]
    addSuffix _ _ recordName = [TopName . mkName $ nameBase recordName ++ "A"]
