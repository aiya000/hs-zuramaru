{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Elin.Type
  ( SourceCode
  , EliAST (..)
  , EliLiteral (..)
  ) where

import Data.Map.Lazy (Map)
import Data.Text (Text)

-- |
-- eliningen source code,
-- this type doesn't mean the file of source code
type SourceCode = Text

data EliAST = EliLit EliLiteral
  deriving (Show)


--NOTE: Can I use newtype instead of data ?
-- | Dynamic typed value
data EliLiteral = forall a. Show a => EliLiteral a
instance Show EliLiteral where
  show (EliLiteral x) = show x

-- | The identifier for function, variable and etc
type EliIdentifier = String

-- | Dynamic scope (global)
type EliScope = Map EliIdentifier EliLiteral
