module Elin.Type
  ( SourceCode
  , Token
  , SExpr (..)
  , Identifier
  ) where

import Data.Text (Text)
import qualified Text.Megaparsec as P

-- |
-- eliningen source code,
-- this type doesn't mean the file of source code
type SourceCode = Text

-- | The source code tokens is Text in SourceCode
type Token = P.Token Text

-- | n-ary tree and terms
data SExpr = Cons SExpr SExpr  -- ^ Appending list and list
           | Nil  -- ^ A representation of empty list
           | Symbol Identifier  -- ^ Some element
  deriving (Show)

-- | Symbol identifier
type Identifier = Text
