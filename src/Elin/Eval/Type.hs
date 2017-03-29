{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- | TODO
module Elin.Eval.Type where

import Data.Text (Text)

-- | EliIdent for variable, macro, and function
type EliIdent = Text

-- | A dynamic typed value
data EliValue = forall a. Show a => EliValue a
instance Show EliValue where
  show (EliValue x) = show x
