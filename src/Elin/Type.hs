-- | Common types for eliningen
module Elin.Type
  ( SourceCode
  , Token
  , SExpr (..)
  ) where

import Data.Text (Text)
import qualified Text.Megaparsec as P

-- |
-- Format of eliningen source code
-- (this doesn't mean the file path of source code)
type SourceCode = Text

-- | Format of parser token
type Token = P.Token Text

-- | n-ary tree and terms
data SExpr = Cons SExpr SExpr -- ^ Appending list and list
           | Nil              -- ^ A representation of empty list
           | Symbol Text      -- ^ A name of variable, function or macro
  deriving (Show)
