{-# LANGUAGE OverloadedStrings #-}

-- | The preprocessor between 'CallowSExpr' and 'SExpr'
module Maru.Preprocessor
  ( preprocess
  ) where

import Maru.Type

-- |
-- Process a 'SExpr' to the desired form before 'Maru.Eval.eval'
--
-- Regard the symbol of "quote" to a quote macro
--
-- >>> preprocess $ Cons' (AtomSymbol' "quote") (Cons' (AtomInt' 10) Nil')
-- Quote (AtomInt 10)
preprocess :: CallowSExpr -> SExpr
preprocess (Cons' (AtomSymbol' "quote") (Cons' x Nil')) = Quote $ growUp x
preprocess x = growUp x
