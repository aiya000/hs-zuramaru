{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Elin.Type
  ( SourceCode
  , EliAST (..)
  , EliLiteral (..)
  ) where

import Data.Text (Text)

-- |
-- eliningen source code,
-- this type doesn't mean the file of source code
type SourceCode = Text

data EliAST = forall a. Show a => EliLit (EliLiteral a)  -- ^ dynamic typing
instance Show EliAST where
  show (EliLit x) = show x

newtype EliLiteral a = EliLiteral { unEliLiteral :: a }
  deriving (Show)
