{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | These type is used in Parser
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

-- | A log for ElinParser
data ParseLog = Message Text     -- ^ Simple message
              | ParsedItem Text  -- ^ Ex: '(', ''', ')' or some symbol
  deriving (Show)

-- | ElinParser's current state
data ElinState = ElinState
  { _parseLogs      :: [ParseLog]
  , _parseNestLevel :: Int
  } deriving (Show)
makeLenses ''ElinState

-- | Parser with logging
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

-- | Append a message to head of _parseLogs in the parsing
tell :: ParseLog -> ElinParser ()
tell log = parseLogs %= (log:)

-- |
-- `tell` text as Message.
-- the parseNestLevel is appended as indent
tellMsg :: Text -> ElinParser ()
tellMsg txt = do
  indentLevel <- gets _parseNestLevel
  let indent = T.pack $ replicate indentLevel '\t'
      txt'   = indent <> txt
  tell $ Message txt'

-- | `tell` text as ParsedItem
tellItem :: Text -> ElinParser ()
tellItem = tell . ParsedItem

-- | Add 1 to parseNestLevel
increaseNestLevel :: ElinParser ()
increaseNestLevel = parseNestLevel += 1

-- | Subtract 1 to parseNestLevel
decreaseNestLevel :: ElinParser ()
decreaseNestLevel = parseNestLevel -= 1
