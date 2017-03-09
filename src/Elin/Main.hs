module Elin.Main (defaultMain) where

import Control.Monad ((<$!>))
import Safe (headMay)
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Elin.Parser as EP


-- |
-- Parse specified source code.
-- And evaluate its result
defaultMain :: IO ()
defaultMain = do
  maybeFilePath <- headMay <$> getArgs
  case maybeFilePath of
    Nothing -> putStrLn description
    Just x  -> T.pack <$!> readFile x >>= EP.parseTest


-- TODO
description :: String
description = "TODO (description)"
