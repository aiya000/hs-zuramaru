{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The type for Maru.Parser
module Maru.Type.Parser
  ( ParseLog (..)
  , ParseLogs
  , MaruParser
  , runMaruParser
  , tell'
  , ParseErrorResult
  ) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Fail (MonadFail)
import Control.Monad.State.Class (MonadState, modify)
import Control.Monad.State.Lazy (State, runState)
import Data.Sequence (Seq, (|>))
import Data.String (IsString)
import Data.Text (Text)
import Maru.Type.SExpr
import Text.Megaparsec (ParsecT, ParseError, Dec, runParserT)
import Text.Megaparsec.Prim (MonadParsec)
import qualified Data.Text as T
import qualified GHC.Exts as L

-- | The parse result of the failure
type ParseErrorResult = ParseError MaruToken Dec

-- | A log message of the parsing, for debug
newtype ParseLog = ParseLog { unParseLog :: Text }
  deriving (IsString, Monoid)

instance Show ParseLog where
  show = show . T.unpack . unParseLog

type ParseLogs = Seq ParseLog

-- |
-- A parser for the code of maru
--
-- NOTE:
-- This `MonadState` instance is instead of `MonadWriter`,
-- because `ParsecT` is not the `MonadWriter` instance.
newtype MaruParser a = MaruParser { unMaruParser :: ParsecT Dec Text (State ParseLogs) a }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadState ParseLogs
           , MonadParsec Dec Text
           , MonadFail
           )

-- |
-- Run parser and extract result and logs.
runMaruParser :: MaruParser a -> Text -> (Either ParseErrorResult a, [ParseLog])
runMaruParser parser source =
  let bareness = unMaruParser parser
      (result, logs) = flip runState [] $ runParserT bareness "" source
  in (result, L.toList logs)

-- | Append a log to head of _parseLogs in the parsing
tell' :: ParseLog -> MaruParser ()
tell' log = modify (|> log)
