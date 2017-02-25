module Elin.Main (defaultMain) where

import Control.Exception.Safe (try, SomeException)
import Control.Monad ((<$!>))
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Elin.Parser as EP


defaultMain :: IO ()
defaultMain = do
  --FIXME: try didn't catch the error
  eitherFilePath <- try (head <$> getArgs)
  case eitherFilePath of
    Left  e -> putStrLn $ "error: " ++ show (e :: SomeException)
    Right a -> T.pack <$!> readFile a >>= EP.parseTest
