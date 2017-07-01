{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Maru.Main
  ( run
  , runRepl
  ) where

import Control.Monad ((<$!>), mapM, when)
import Control.Monad.Cont (ContT(..), runContT)
import Data.Maybe (isJust)
import Data.Text (Text)
import Maru.Eval (Env)
import Maru.Type (SExpr, ParseLog, ParseErrorResult)
import Safe (headMay)
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Maru.Eval as Eval
import qualified Maru.Parser as Parser
import qualified Maru.Type as MT
import qualified System.Console.Readline as R


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


-- | @n@ can be flipped up to the top in @flipUp@
class (Functor m, Functor n) => FlipUp m n where
  flipUp :: m (n a) -> n (m a)

--NOTE: I may can use MMonad's embed with MaybeT, if evalPhase has @Bool -> Text -> MaybeT IO ([ParseLog] `StandBy` Either ParseErrorResult SExpr)@
instance FlipUp Maybe IO where
  flipUp :: Maybe (IO a) -> IO (Maybe a)
  flipUp Nothing  = return Nothing
  flipUp (Just x) = Just <$> x


type EvalResult = Either ParseErrorResult SExpr


-- |
-- Parse specified source code.
-- And evaluate its result
run :: IO ()
run = do
  maybeFilePath <- headMay <$> getArgs
  case maybeFilePath of
    Nothing -> putStrLn description
    Just x  -> do
      code <- T.pack <$!> readFile x
      case Parser.debugParse code of
        resultWithLogs@(Left _, _) -> Parser.prettyPrint resultWithLogs
        (Right sexpr, _)           -> Eval.eval sexpr >>= TIO.putStrLn . MT.visualize
  where
    --TODO: Write
    description :: String
    description = "TODO (description)"

-- |
-- Startup REPL.
-- Parse and evaluate successively.
runRepl :: IO ()
runRepl = continue ()
  where
    -- An argument is needed by the loop, it can be anything
    continue :: () -> IO ()
    continue () = flip runContT continue . ContT $ repl Eval.initialEnv

-- |
-- Do 'Loop' of 'Read', 'eval', and 'Print',
-- for @ContT@.
repl :: Env -> (() -> IO ()) -> IO ()
repl env continue = do
  loopIsRequired <- rep env
  when loopIsRequired $ continue ()
  where
    -- Do 'Read', 'Eval', and 'Print' of 'REPL'.
    -- Return False if Ctrl+d is input.
    -- Return True otherwise.
    rep :: Env -> IO Bool
    rep env = do
      maybeSome <- headMay <$> getArgs  --TODO: Use some option library
      let inDebugMode = isJust maybeSome
      maybeInput          <- readPhase
      maybeResultWithLogs <- flipUp (evalPhase inDebugMode <$> maybeInput)
      maybeUnit           <- flipUp (printPhase <$> maybeResultWithLogs)
      return $ iso maybeUnit

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
    evalPhase :: Bool -> Text -> IO ([ParseLog] `StandBy` EvalResult)
    evalPhase False code =
      case Parser.parse code of
        Left parseErrorResult -> return . Lonely $ Left parseErrorResult
        Right sexpr           -> Lonely . Right <$> Eval.eval sexpr

    evalPhase True code =
      case Parser.debugParse code of
        (Left errorResult, _) -> return . Lonely $ Left errorResult 
        (Right sexpr, logs)   -> With logs . Right <$> Eval.eval sexpr

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
-- An isomorphism of Maybe () and Bool,
-- Just () ~= True, Nothing ~= False.
iso :: Maybe () -> Bool
iso (Just _) = True
iso Nothing  = False
