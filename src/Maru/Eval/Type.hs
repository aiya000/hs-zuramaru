{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- | TODO
module Maru.Eval.Type where

import Data.Text (Text)

-- | MaruIdent for variable, macro, and function
type MaruIdent = Text

-- |
-- Num of a term's argument
-- Ex: print is 1, cons is 2, defun is 3 (ConstNum)
-- and a value is 0 (`NoArg`), let is variant(Variant)
data ArgumentNum = ConstNum Int
                 | VariantNum
                 | NoArg

-- | A dynamic typed value
data MaruTerm = forall a. MaruTerm
  { termArgNum :: ArgumentNum
  , termValue  :: a
  }
