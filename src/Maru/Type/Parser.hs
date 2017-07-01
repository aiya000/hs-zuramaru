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
  , tellItem
  , increaseNestLevel
  , decreaseNestLevel
  , ParseErrorResult
  ) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.State.Class (MonadState, gets)
import Control.Monad.State.Lazy (State, runState)
import Data.Monoid ((<>))
import Data.Text (Text)
import Lens.Micro.Mtl ((%=), (+=), (-=))
import Lens.Micro.TH (makeLenses)
import Maru.Type.SExpr
import Text.Megaparsec (ParsecT, ParseError, Dec, runParserT)
import Text.Megaparsec.Prim (MonadParsec)
import qualified Data.Text as T

type ParseErrorResult = ParseError MaruToken Dec


-- | A log of MaruParser
data ParseLog = Message Text     -- ^ Simple message
              | ParsedItem Text  -- ^ Ex: '(', ''', ')' or some symbol
  deriving (Show)

-- | Current state of MaruParser
data MaruState = MaruState
  { _parseLogs      :: [ParseLog]
  , _parseNestLevel :: Int
  } deriving (Show)
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
-- Apply text to `tell` as Message.
-- the parseNestLevel is appended as indent to text automatically
tellMsg :: Text -> MaruParser ()
tellMsg txt = do
  indentLevel <- gets _parseNestLevel
  let indent = T.pack $ replicate indentLevel '\t'
      txt'   = indent <> txt
  tell $ Message txt'

-- | Apply text to `tell` as ParsedItem
tellItem :: Text -> MaruParser ()
tellItem = tell . ParsedItem

-- | Add 1 to parseNestLevel of state
increaseNestLevel :: MaruParser ()
increaseNestLevel = parseNestLevel += 1

-- | Subtract 1 to parseNestLevel of state
decreaseNestLevel :: MaruParser ()
decreaseNestLevel = parseNestLevel -= 1
