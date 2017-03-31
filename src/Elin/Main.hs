{-# LANGUAGE OverloadedStrings #-}

module Elin.Main
  ( run
  , repl
  ) where

import Control.Exception.Safe (try, IOException)
import Control.Monad ((<$!>), mapM)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Safe (headMay)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Elin.Eval as EV
import qualified Elin.Parser as EP


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
  putStr' "elin> "
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
    -- Read stdin.
    -- Return Nothing if stdin gives to interrupt
    readPhase :: IO (Maybe Text)
    readPhase = do
      input <- try (T.pack <$> getLine)
      case input of
        Left  e -> if isKeyboardInterrupt e
                      then return Nothing
                      else return . Just $ tShow e
        Right a -> return $ Just a

    -- Evaluate 'read' result.
    evalPrintPhase :: Text -> IO ()
    evalPrintPhase code = do
      case EP.parse code of
        Left errorResult -> tPutStrLn $ EP.parseErrorPretty errorResult --TODO: Optimize error column and representation
        Right ast        -> tPrint ast

--NOTE: How is another cool way ?
-- | Judge what IOException is result of stdio interruption
isKeyboardInterrupt :: IOException -> Bool
isKeyboardInterrupt e = show e == "<stdin>: hGetLine: end of file"

-- | Convert Show instance as Text
tShow :: Show a => a -> Text
tShow = T.pack . show

-- |
-- Regard String as Text.
-- And do putStrLn it
tPutStrLn :: String -> IO ()
tPutStrLn = TIO.putStrLn . T.pack

-- |
-- Convert Show instance as Text.
-- And do Data.Text.IO.putStrLn it
tPrint :: Show a => a -> IO ()
tPrint = TIO.putStrLn . tShow

-- | Do putStr directly (Don't lazy)
putStr' :: String -> IO ()
putStr' x = putStr x >> hFlush stdout
