{-# LANGUAGE OverloadedStrings #-}

module Elin.Main
  ( run
  , repl
  ) where

import Control.Monad ((<$!>), mapM)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Safe (headMay)
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Elin.Eval as EV
import qualified Elin.Parser as EP
import qualified Elin.Type as ET
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
      case EP.debugParse code of
        y@(Left _, _)    -> EP.prettyPrint y
        --(Right sexpr, _) -> EV.eval sexpr
        y@(Right sexpr, _) -> EP.prettyPrint y >> EV.eval sexpr
  where
    -- TODO
    description :: String
    description = "TODO (description)"

-- |
-- Startup REPL.
-- Parse and evaluate successively
repl :: IO ()
repl = do
  escapeIsRequired <- rep
  if escapeIsRequired
     then putStrLn "Bye"
     else repl

-- | Read, eval and print
rep :: IO Bool
rep = do
  maybeInput <- readPhase
  maybeUnit  <- mapM evalPrintPhase maybeInput
  return $ isNothing maybeUnit
  where
    -- Read line from stdin.
    -- If stdin gives to interrupt, return Nothing.
    -- If it's not, return it and it is added to history file
    readPhase :: IO (Maybe Text)
    readPhase = do
      maybeInput <- R.readline "elin> "
      mapM R.addHistory maybeInput
      return $ fmap T.pack maybeInput

    -- Evaluate 'read' result.
    evalPrintPhase :: Text -> IO ()
    evalPrintPhase code = do
      case EP.parse code of
        Left errorResult -> tPutStrLn $ EP.parseErrorPretty errorResult --TODO: Optimize error column and representation
        Right ast        -> TIO.putStrLn $ ET.lispnize ast

-- |
-- Regard String as Text.
-- And do putStrLn it
tPutStrLn :: String -> IO ()
tPutStrLn = TIO.putStrLn . T.pack
