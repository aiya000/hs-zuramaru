{-# LANGUAGE OverloadedStrings #-}

-- | Common types for zuramaru
module Maru.Type
  ( SourceCode
  , MaruToken
  , SExpr (..)
  , MaruTerm (..)
  , scottEncode
  , toSyntax
  ) where

import Data.List (foldl')
import Data.Monoid ((<>))
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
data SExpr = Cons SExpr SExpr -- ^ Appending list and list
           | Nil              -- ^ A representation of empty list
           | Quote SExpr      -- ^ For lazy evaluation
           | Atom MaruTerm    -- ^ Some term item
  deriving (Show)

-- | A literal, a name of variable, function or macro for zuramaru language
data MaruTerm = TermInt Int     -- ^ Integer literal
              | TermSymbol Text -- ^ Name of variable, function or macro. (this includes nil)
  deriving (Show)

instance TextShow MaruTerm where
  showb (TermInt x)    = showb x
  showb (TermSymbol x) = TS.fromText x


-- | Concatenate SExpr by Cons
--
-- >>> let xs = [(Atom (TermInt 1)), (Cons (Cons (Atom (TermInt 2)) (Cons (Atom (TermInt 3)) Nil)) Nil)] -- [1, (2 3)]
-- >>> scottEncode xs
-- Cons (Atom (TermInt 1)) (Cons (Cons (Cons (Atom (TermInt 2)) (Cons (Atom (TermInt 3)) Nil)) Nil) Nil)
-- >>> let ys = [Atom (TermInt 1), Atom (TermInt 2), Atom (TermInt 3)] -- [1, 2, 3]
-- >>> scottEncode ys
-- Cons (Atom (TermInt 1)) (Cons (Atom (TermInt 2)) (Cons (Atom (TermInt 3)) Nil))
-- >>> let zs = [Atom (TermInt 1), Nil] -- [1, ()]
-- >>> scottEncode zs
-- Cons (Atom (TermInt 1)) (Cons Nil Nil)
scottEncode :: [SExpr] -> SExpr
scottEncode [] = Nil
scottEncode (x:xs) = Cons x $ scottEncode xs

--TODO: Add (Quote _) pattern after Quote parser and Quote Evaluator is implmenented
-- | The inverse function of @scottEncode@
scottDecode :: SExpr -> [SExpr]
scottDecode (Cons x y) = x : scottDecode y
scottDecode Nil = []
scottDecode (Atom x) = [Atom x]


--TODO: Add (Quote _) pattern after Quote parser and Quote Evaluator is implmenented
-- |
-- Convert AST to human readable syntax.
-- This maybe the inverse function of the parser.
toSyntax :: SExpr -> Text
toSyntax (Cons x y) =
  let innerListSyntax = foldl' (<<>>) "" $ map toSyntax $ scottDecode y
  in "(" <> toSyntax x <<>> innerListSyntax <> ")"
  where
    a  <<>> "" = a
    "" <<>> b  = b
    a  <<>> b  = a <> " " <> b

toSyntax (Atom x) = showt x
toSyntax Nil = "()"
