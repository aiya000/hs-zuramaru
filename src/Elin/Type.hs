{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Elin.Type where

import Data.Map.Lazy (Map)
import Data.Text (Text)

-- |
-- eliningen source code,
-- this type doesn't mean the file of source code
type SourceCode = Text

-- | n-ary tree and terms
data SExpr = SExpr [SExpr]  -- ^ Simple n-ary tree
           | SQuote SExpr  -- ^ An evaluation Of specify SExpr is delayed
           | forall a. (EliLiteral a, Show a) => SLit a
           | SName Identifier  -- ^ The name of variable and function
instance Show SExpr where
  show (SExpr xs) = "S( " ++ show xs ++ " )"
  show (SQuote x) = "'" ++ show x
  show (SLit   x) = show x
  show (SName  x) = show x

-- | The identifier for function and variable
type Identifier = String

-- | eliningen's value terms
class EliLiteral a
instance EliLiteral Int
instance EliLiteral Float
instance EliLiteral Char
instance EliLiteral Bool
instance EliLiteral String
