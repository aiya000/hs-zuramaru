module Elin.Main (defaultMain) where

import Control.Monad ((<$!>))
import Safe (headMay)
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Elin.Eval as EV
import qualified Elin.Parser as EP


-- |
-- Parse specified source code.
-- And evaluate its result
defaultMain :: IO ()
defaultMain = do
  maybeFilePath <- headMay <$> getArgs
  case maybeFilePath of
    Nothing -> putStrLn description
    Just x  -> do
      code <- T.pack <$!> readFile x
      case EP.debugParse code of
        y@(Left _, _)    -> EP.parseErrorPretty y
        --(Right sexpr, _) -> EV.eval sexpr
        y@(Right sexpr, _) -> EP.parseErrorPretty y >> EV.eval sexpr


-- TODO
description :: String
description = "TODO (description)"
