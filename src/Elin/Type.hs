{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Elin.Type
  ( SourceCode
  , SExpr (..)
  , FuncName
  , SyntaxType (..)
  , EliLiteral (..)
  , EliIdentifier
  , EliScope
  ) where

import Data.Map.Lazy (Map)
import Data.Text (Text)

-- |
-- eliningen source code,
-- this type doesn't mean the file of source code
type SourceCode = Text

-- | n-ary tree with node type
data SExpr = SNil                           -- ^ ex: ()
           | SLit EliLiteral SExpr          -- ^ ex: (10) ~ SLit (EliInt 10) Nil
           | SSyntax SyntaxType SExpr SExpr -- ^ ex: (let a (10)) ~ SSyntax Let (SLit (EliInt 10) Nil) Nil
           | SVar VarName SExpr SExpr       -- ^ ex: (a (10)) ~ SVar "a" (SLit (EliInt 10) Nil) Nil
           | SFunc FuncName SExpr SExpr     -- ^ ex: (concat [10] [20 30]) ~ SFunc "concat" (SLit (EliInt 10) Nil) (SLit (EliInt 20) (SLit (EliInt 30) Nil)
  deriving (Show)

-- | The tag for a function
type FuncName = EliIdentifier

-- | The tag for a variable
type VarName  = EliIdentifier

-- | The tag for a syntax
data SyntaxType = Let
                | Def
                | Lambda
                | Defn
  deriving (Show)


-- | A literal type
data EliLiteral = EliInt Int
                | EliFloat Float
                | EliBool Bool
                | EliChar Char
  deriving (Show)

-- | The identifier for function, variable and etc
type EliIdentifier = String

-- | Global dynamic scope
type EliScope = Map EliIdentifier SExpr
