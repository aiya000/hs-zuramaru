-- | 'SExpr' preprocessor
module Maru.Preprocessor
  ( preprocess
  ) where

import Maru.Type (SExpr(..))

-- | Process a 'SExpr' to the desired form before 'Maru.Eval.eval'
preprocess :: SExpr -> SExpr
preprocess = undefined
