{-# LANGUAGE OverloadedStrings #-}

module Maru.Main
  ( run
  , repl
  ) where

import Control.Monad ((<$!>), mapM, when)
import Control.Monad.Cont (ContT(..), runContT)
import Data.Maybe (isNothing, isJust)
import Data.Text (Text)
import Safe (headMay)
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Maru.Eval as Eval
import qualified Maru.Parser as Parser
import qualified Maru.Type as MT
import qualified System.Console.Readline as R


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
        y@(Left _, _)      -> Parser.prettyPrint y
        y@(Right sexpr, _) -> Parser.prettyPrint y >> Eval.eval sexpr
  where
    --TODO: Write
    description :: String
    description = "TODO (description)"

-- |
-- Startup REPL.
-- Parse and evaluate successively.
repl :: IO ()
repl = continue ()
  where
    -- An argument is needed by the loop, it can be anything
    continue :: () -> IO ()
    continue () = flip runContT continue $ ContT rep

-- | Read, eval and print
rep :: (() -> IO ()) -> IO ()
rep continue = do
  maybeSome <- headMay <$> getArgs  --TODO: Use some option library
  let inDebugMode = isJust maybeSome
  maybeInput <- readPhase
  maybeUnit  <- mapM (evalPrintPhase inDebugMode) maybeInput
  when (not $ isNothing maybeUnit) $ continue ()
  where
    -- Read line from stdin.
    -- If stdin gives to interrupt, return Nothing.
    -- If it's not, return it and it is added to history file
    readPhase :: IO (Maybe Text)
    readPhase = do
      maybeInput <- R.readline "zuramaru> "
      mapM R.addHistory maybeInput
      return $ fmap T.pack maybeInput

    -- Evaluate 'read' result
    evalPrintPhase :: Bool -> Text -> IO ()
    evalPrintPhase False code = do
      case Parser.parse code of
        Left errorResult -> tPutStrLn $ Parser.parseErrorPretty errorResult  --TODO: Optimize error column and representation
        Right ast        -> TIO.putStrLn $ MT.toSyntax ast

    -- Evaluate 'read' result with debugging
    evalPrintPhase True code = do
      case Parser.debugParse code of
        x@(Left _, _)  -> Parser.prettyPrint x
        (Right ast, _) -> do
          tPrint ast  -- Show ast directly
          TIO.putStrLn $ MT.toSyntax ast


-- |
-- Regard String as Text.
-- And apply putStrLn to it
tPutStrLn :: String -> IO ()
tPutStrLn = TIO.putStrLn . T.pack

-- |
-- Convert a to Text.
-- And apply Data.Text.IO.putStrLn to it
tPrint :: Show a => a -> IO ()
tPrint = TIO.putStrLn . T.pack . show
