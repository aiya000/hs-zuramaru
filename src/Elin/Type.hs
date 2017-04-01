{-# LANGUAGE OverloadedStrings #-}

-- | Common types for eliningen
module Elin.Type
  ( SourceCode
  , ElinToken
  , SExpr (..)
  , ElinTerm (..)
  , lispnize
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as P

-- |
-- Format of eliningen source code
-- (this doesn't mean the file path of source code)
type SourceCode = Text

-- | Format of parser token
type ElinToken = P.Token Text

-- | n-ary tree and terms
data SExpr = Cons SExpr SExpr  -- ^ Appending list and list
           | Nil               -- ^ A representation of empty list
           | Quote SExpr       -- ^ For lazy evaluation
           | TermItem ElinTerm -- ^ Some term item
  deriving (Show)

-- | A literal, a name of variable, function or macro for eliningen language
data ElinTerm = TermInt Int    -- ^ Integer literal
              | TermName Text  -- ^ Name of variable, function or macro
  deriving (Show)


-- | Convert SExpr to readable lisp syntax
lispnize :: SExpr -> Text
lispnize Nil = "nil"
lispnize (Quote x) = "'" <> lispnize x
lispnize (TermItem (TermInt  x)) = T.pack . show $ x
lispnize (TermItem (TermName x)) = x
lispnize (Cons l r) = "(" <> lispnize l <> " " <> lispnize r <> ")"
