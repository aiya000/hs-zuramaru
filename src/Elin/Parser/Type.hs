{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The type for Elin.Parser
module Elin.Parser.Type
  ( ParseLog (..)
  , ElinState (..)
  , parseLogs
  , parseNestLevel
  , ElinParser
  , runElinParser
  , tell
  , tellMsg
  , tellItem
  , increaseNestLevel
  , decreaseNestLevel
  ) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.State.Class (MonadState, gets)
import Control.Monad.State.Lazy (State, runState)
import Data.Monoid ((<>))
import Data.Text (Text)
import Elin.Type (Token)
import Lens.Micro.Mtl ((%=), (+=), (-=))
import Lens.Micro.TH (makeLenses)
import Text.Megaparsec (ParsecT, ParseError, Dec, runParserT)
import Text.Megaparsec.Prim (MonadParsec)
import qualified Data.Text as T

-- | A log of ElinParser
data ParseLog = Message Text     -- ^ Simple message
              | ParsedItem Text  -- ^ Ex: '(', ''', ')' or some symbol
  deriving (Show)

-- | Current state of ElinParser
data ElinState = ElinState
  { _parseLogs      :: [ParseLog]
  , _parseNestLevel :: Int
  } deriving (Show)
makeLenses ''ElinState

-- | Parser with parsing logs
newtype ElinParser a = ElinParser { _runElinParser :: ParsecT Dec Text (State ElinState) a }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadState ElinState, MonadParsec Dec Text
           )

-- | Run parser and extract result and logs
runElinParser :: ElinParser a -> Text -> (Either (ParseError Token Dec) a, [ParseLog])
runElinParser parser source =
  let initialState = ElinState [] 0
      bareness     = _runElinParser parser
      (result, ElinState logs _) = flip runState initialState $ runParserT bareness "" source
  in (result, reverse logs)  --NOTE: `reverse` fixes reversed [ParseLog] order

-- |
-- Append a log to head of _parseLogs in the parsing.
--
-- Attention: The new log is put to top, not bottom !
tell :: ParseLog -> ElinParser ()
tell log = parseLogs %= (log:)

-- |
-- Apply text to `tell` as Message.
-- the parseNestLevel is appended as indent to text automatically
tellMsg :: Text -> ElinParser ()
tellMsg txt = do
  indentLevel <- gets _parseNestLevel
  let indent = T.pack $ replicate indentLevel '\t'
      txt'   = indent <> txt
  tell $ Message txt'

-- | Apply text to `tell` as ParsedItem
tellItem :: Text -> ElinParser ()
tellItem = tell . ParsedItem

-- | Add 1 to parseNestLevel of state
increaseNestLevel :: ElinParser ()
increaseNestLevel = parseNestLevel += 1

-- | Subtract 1 to parseNestLevel of state
decreaseNestLevel :: ElinParser ()
decreaseNestLevel = parseNestLevel -= 1
