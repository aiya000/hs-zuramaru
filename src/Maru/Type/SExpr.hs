{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Common types for zuramaru
module Maru.Type.SExpr
  ( SourceCode
  , MaruToken
  , SExpr (..)
  , SExprLike (..)
  , AST(..)
  , Symbol (..)
  , scottEncode
  , scottDecode
  ) where

import Data.List (foldl')
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text)
import TextShow (TextShow, showb, showt)
import qualified Text.Megaparsec as P
import qualified TextShow as TS

-- |
-- Format of zuramaru source code
-- (this doesn't mean the file path of source code)
type SourceCode = Text

-- | Format of parser token
type MaruToken = P.Token Text


-- | n-ary tree and terms
data SExpr = Cons SExpr SExpr  -- ^ Appending list and list
           | Nil               -- ^ A representation of empty list
           | Quote SExpr       -- ^ For lazy evaluation
           | AtomInt Int       -- ^ A pattern of the atom for @Int@ (primitive)
           | AtomSymbol Symbol -- ^ A pattern of the atom for @Symbol@ (primitive)
  deriving (Show, Eq)

-- | Same as Show
instance TextShow SExpr where
  showb = TS.fromString . show

-- | A symbol of `MaruEnv`, but this is not meaning a symbol of maru side
newtype Symbol = Symbol { unSymbol :: Text }
  deriving (IsString, Show, Eq, Ord)


-- | @a@ can be represented as @SExpr@
class SExprLike a where
  -- | @a@ can be converted as @SExpr@.
  wrap :: a -> SExpr

instance SExprLike Int where
  wrap = AtomInt

-- | As a symbol
instance SExprLike Text where
  wrap = AtomSymbol . Symbol


-- | The abstract syntax tree
class AST a where
  -- |
  -- Convert an AST to a text of syntax.
  --
  -- e.g. @(Cons 10 (Cons 20 Nil))@ to "(10 20)"
  visualize :: a -> Text

instance AST SExpr where
  --TODO: Add (Quote _) pattern after Quote parser and Quote Evaluator is implmenented
  -- |
  -- Convert AST to human readable syntax.
  -- This maybe the inverse function of the parser.
  visualize (Cons x y) =
    let innerListSyntax = foldl' (<<>>) "" $ map visualize $ scottDecode y
    in "(" <> visualize x <<>> innerListSyntax <> ")"
    where
      a  <<>> "" = a
      "" <<>> b  = b
      a  <<>> b  = a <> " " <> b
  visualize Nil         = "()"
  visualize (Quote _)   = error "TODO for Quote"
  visualize (AtomSymbol (Symbol x)) = x
  visualize (AtomInt x)             = showt x


-- | Concatenate SExpr by Cons
--
-- >>> let xs = [(AtomInt 1), (Cons (Cons (AtomInt 2) (Cons (AtomInt 3) Nil)) Nil)] -- [1, (2 3)]
-- >>> scottEncode xs
-- Cons (AtomInt 1) (Cons (Cons (Cons (AtomInt 2) (Cons (AtomInt 3) Nil)) Nil) Nil)
-- >>> let ys = [AtomInt 1, AtomInt 2, AtomInt 3] -- [1, 2, 3]
-- >>> scottEncode ys
-- Cons (AtomInt 1) (Cons (AtomInt 2) (Cons (AtomInt 3) Nil))
-- >>> let zs = [AtomInt 1, Nil] -- [1, ()]
-- >>> scottEncode zs
-- Cons (AtomInt 1) (Cons Nil Nil)
scottEncode :: [SExpr] -> SExpr
scottEncode [] = Nil
scottEncode (x:xs) = Cons x $ scottEncode xs

--TODO: Add (Quote _) pattern after Quote parser and Quote Evaluator is implmenented
-- | The inverse function of @scottEncode@
--
-- >>> let xs = Cons (AtomInt 1) (Cons (AtomInt 2) Nil)
-- >>> scottDecode xs
-- [AtomInt 1,AtomInt 2]
-- >>> scottDecode $ Cons (AtomInt 10) Nil
-- [AtomInt 10]
-- >>> scottDecode $ Cons Nil Nil
-- [Nil]
scottDecode :: SExpr -> [SExpr]
scottDecode (Cons x y) = x : scottDecode y
scottDecode Nil = []
scottDecode (AtomSymbol x) = [AtomSymbol x]
scottDecode (AtomInt x)    = [AtomInt x]
scottDecode (Quote _)      = error "TODO for Quote"
