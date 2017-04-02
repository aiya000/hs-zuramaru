{-# LANGUAGE OverloadedStrings #-}

-- | Common types for zuramaru
module Maru.Type
  ( SourceCode
  , MaruToken
  , SExpr (..)
  , MaruTerm (..)
  , lispnize
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as P

-- |
-- Format of zuramaru source code
-- (this doesn't mean the file path of source code)
type SourceCode = Text

-- | Format of parser token
type MaruToken = P.Token Text

-- | n-ary tree and terms
data SExpr = Cons SExpr SExpr -- ^ Appending list and list
           | Nil              -- ^ A representation of empty list
           | Quote SExpr      -- ^ For lazy evaluation
           | Atom MaruTerm    -- ^ Some term item
  deriving (Show)

-- | A literal, a name of variable, function or macro for zuramaru language
data MaruTerm = TermInt Int    -- ^ Integer literal
              | TermSymbol Text  -- ^ Name of variable, function or macro. (this includes nil)
  deriving (Show)


-- | Convert SExpr to readable lisp syntax
lispnize :: SExpr -> Text
lispnize Nil = ""
lispnize (Atom (TermInt    x)) = T.pack . show $ x
lispnize (Atom (TermSymbol x)) = x
lispnize (Quote x)  = "'" <> lispnize x
lispnize (Cons l r) = "(" <> lispnize l <> " " <> lispnize r <> ")"
