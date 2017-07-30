{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Monad (MonadPlus)
import Control.Monad.State.Class (MonadState, gets)
import Control.Monad.State.Lazy (State, runState)
import Data.Monoid ((<>), Monoid)
import Data.String (IsString)
import Data.Text (Text)
import Lens.Micro.Mtl ((%=), (+=), (-=))
import Lens.Micro.TH (makeLenses)
import Maru.Type.SExpr
import Text.Megaparsec (ParsecT, ParseError, Dec, runParserT)
import Text.Megaparsec.Prim (MonadParsec)
import qualified Data.Text as T

type ParseErrorResult = ParseError MaruToken Dec

-- | A log message of the parsing, for debug
newtype ParseLog = ParseLog { unParseLog :: Text }
  deriving (IsString, Monoid)

-- | Current state of MaruParser
data MaruState = MaruState
  { _parseLogs      :: [ParseLog]
  , _parseNestLevel :: Int
  }
makeLenses ''MaruState

-- | Parser with parsing logs
newtype MaruParser a = MaruParser { _runMaruParser :: ParsecT Dec Text (State MaruState) a }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadState MaruState, MonadParsec Dec Text
           )

-- | Run parser and extract result and logs
runMaruParser :: MaruParser a -> Text -> (Either (ParseError MaruToken Dec) a, [ParseLog])
runMaruParser parser source =
  let initialState = MaruState [] 0
      bareness     = _runMaruParser parser
      (result, MaruState logs _) = flip runState initialState $ runParserT bareness "" source
  in (result, reverse logs)  --NOTE: `reverse` fixes reversed [ParseLog] order

-- |
-- Append a log to head of _parseLogs in the parsing.
--
-- Attention: The new log is put to top, not bottom !
tell :: ParseLog -> MaruParser ()
tell log = parseLogs %= (log:)

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
