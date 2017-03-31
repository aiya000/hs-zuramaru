module Elin.Main
  ( run
  , repl
  ) where

import Control.Monad ((<$!>))
import Safe (headMay)
import System.Environment (getArgs)
import qualified Data.Text as T
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
        y@(Left _, _)    -> EP.parseErrorPretty y
        --(Right sexpr, _) -> EV.eval sexpr
        y@(Right sexpr, _) -> EP.parseErrorPretty y >> EV.eval sexpr
  where
    -- TODO
    description :: String
    description = "TODO (description)"

-- |
-- Startup REPL.
-- Parse and evaluate successively
repl :: IO ()
repl = undefined
