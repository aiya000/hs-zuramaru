-- | The preprocessor between 'CallowSExpr' and 'SExpr'
module Maru.Preprocessor
  ( preprocess
  ) where

import Maru.Type (CallowSExpr(..), SExpr(..))

-- | Process a 'SExpr' to the desired form before 'Maru.Eval.eval'
preprocess :: CallowSExpr -> SExpr
preprocess = undefined
