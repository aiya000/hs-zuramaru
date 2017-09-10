{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The type for Maru.Parser
module Maru.Type.Parser
  ( ParseLog (..)
  , MaruState (..)
  , parseLogs
  , parseNestLevel
  , MaruParser
  , runMaruParser
  , tell
  , tellMsg
  , increaseNestLevel
  , decreaseNestLevel
  , ParseErrorResult
  ) where

import Control.Applicative (Alternative)
import Control.Lens ((%=), (+=), (-=), makeLenses)
import Control.Monad (MonadPlus)
import Control.Monad.State.Class (MonadState, gets)
import Control.Monad.State.Lazy (State, runState)
import Data.Monoid ((<>), Monoid)
import Data.Sequence (Seq, (|>))
import Data.String (IsString)
import Data.Text (Text)
import Maru.Type.SExpr
import Text.Megaparsec (ParsecT, ParseError, Dec, runParserT)
import Text.Megaparsec.Prim (MonadParsec)
import qualified Data.Text as T
import qualified GHC.Exts as L

type ParseErrorResult = ParseError MaruToken Dec

-- | A log message of the parsing, for debug
newtype ParseLog = ParseLog { unParseLog :: Text }
  deriving (IsString, Monoid)

-- | Current state of `MaruParser`
data MaruState = MaruState
  { _parseLogs      :: Seq ParseLog
  , _parseNestLevel :: Int
  }
makeLenses ''MaruState

-- | A parser for the code of maru
newtype MaruParser a = MaruParser { unMaruParser :: ParsecT Dec Text (State MaruState) a }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadState MaruState, MonadParsec Dec Text
           )

-- |
-- Run parser and extract result and logs.
--
-- But `MaruState`'s `_parseLogs` is converted to the list.
runMaruParser :: MaruParser a -> Text -> (Either (ParseError MaruToken Dec) a, [ParseLog])
runMaruParser parser source =
  let initialState = MaruState [] 0
      bareness     = unMaruParser parser
      (result, MaruState logs _) = flip runState initialState $ runParserT bareness "" source
  in (result, L.toList logs)

-- | Append a log to head of _parseLogs in the parsing
tell :: ParseLog -> MaruParser ()
tell log = parseLogs %= (|> log)

-- |
-- Apply text to `tell`.
-- The parseNestLevel is appended as indent to text automatically
tellMsg :: Text -> MaruParser ()
tellMsg txt = do
  indentLevel <- gets _parseNestLevel
  let indent = ParseLog . T.pack $ replicate indentLevel '\t'
  tell $ indent <> ParseLog txt

-- | Add 1 to parseNestLevel of state
increaseNestLevel :: MaruParser ()
increaseNestLevel = parseNestLevel += 1

-- | Subtract 1 to parseNestLevel of state
decreaseNestLevel :: MaruParser ()
decreaseNestLevel = parseNestLevel -= 1
