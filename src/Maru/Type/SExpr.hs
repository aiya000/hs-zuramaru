{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


-- | Common types for zuramaru
module Maru.Type.SExpr
  ( SourceCode
  , MaruToken
  , SExpr(..)
  , isAtomInt
  , unAtomInt
  , isAtomSymbol
  , SExprLike(..)
  , readable
  , MaruSymbol(..)
  , pack
  , unpack
  , scottEncode
  , scottDecode
  , _Cons
  , _Nil
  , _AtomInt
  , _AtomSymbol
  , SExprIntBullet(..)
  , intBullet
  ) where

import Control.Lens (makePrisms)
import Data.List (foldl')
import Data.MonoTraversable (MonoFunctor(..), Element)
import Data.Monoid ((<>))
import Data.Profunctor (dimap)
import Data.Semigroup (Semigroup)
import Data.String (IsString)
import Data.Text (Text)
import TextShow (TextShow, showb, showt)
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified TextShow as TS

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Maru.Parser (parse)

-- |
-- The format for the code of maru.
-- (This doesn't mean a file path of the code.)
type SourceCode = Text

-- | The format for the token of `MaruParser`
type MaruToken = P.Token Text


-- | n-ary tree and terms
data SExpr = Cons SExpr SExpr -- ^ Appending list and list
           | Nil              -- ^ A representation of empty list
           | AtomInt Int      -- ^ A pattern of the atom for @Int@ (primitive)
           | AtomSymbol MaruSymbol -- ^ A pattern of the atom for @MaruSymbol@ (primitive)
  deriving (Show, Eq)

-- |
-- >>> isAtomInt $ AtomInt 10
-- True
-- >>> isAtomInt Nil
-- False
-- >>> isAtomInt $ AtomSymbol ""
-- False
isAtomInt :: SExpr -> Bool
isAtomInt (AtomInt _) = True
isAtomInt _           = False

-- |
-- Extract `Int` from a term of `AtomInt`.
--
-- >>> unAtomInt $ AtomInt 10
-- Just 10
-- >>> unAtomInt $ AtomSymbol ":D"
-- Nothing
-- >>> unAtomInt $ Cons (AtomInt 10) (AtomInt 20)
-- Nothing
unAtomInt :: SExpr -> Maybe Int
unAtomInt (AtomInt x) = Just x
unAtomInt _           = Nothing

-- |
-- >>> isAtomSymbol $ AtomSymbol "x"
-- True
-- >>> isAtomSymbol Nil
-- False
-- >>> isAtomSymbol $ AtomInt 10
-- False
isAtomSymbol :: SExpr -> Bool
isAtomSymbol (AtomSymbol _) = True
isAtomSymbol _              = False

-- | Same as Show
instance TextShow SExpr where
  showb = TS.fromString . show

-- | Shot only the `AtomInt`s by `omap`
newtype SExprIntBullet = SExprIntBullet
  { unSExprIntBullet :: SExpr
  }

type instance Element SExprIntBullet = Int

instance MonoFunctor SExprIntBullet where
  omap f (SExprIntBullet (AtomInt x)) = SExprIntBullet . AtomInt $ f x
  omap _ x = x

-- | Apply by `omap` a function to a `SExprIntBullet` with wrapping and unwrapping
intBullet :: (Int -> Int) -> SExpr -> SExpr
intBullet f xs = dimap SExprIntBullet unSExprIntBullet (omap f) xs


-- | A symbol of `MaruEnv`, but this is not meaning a symbol of maru side
newtype MaruSymbol = MaruSymbol { unMaruSymbol :: Text }
  deriving (IsString, Semigroup, Monoid, Show, Eq, Ord)

-- |
-- Wrap `String`.
-- If you want to wrap `Text`, please use `MaruSymbol` value constructor instead.
pack :: String -> MaruSymbol
pack = MaruSymbol . T.pack

-- | A dual of `pack`
unpack :: MaruSymbol -> String
unpack = T.unpack . unMaruSymbol


-- | 'a' can be represented as `SExpr`
class SExprLike a where
  -- | 'a' can be converted as `SExpr`
  wrap :: a -> SExpr

instance SExprLike Int where
  wrap = AtomInt

--FIXME: Text is not MaruSymbol !!
-- | As a symbol
instance SExprLike Text where
  wrap = AtomSymbol . MaruSymbol


-- |
-- Show `SExpr` as the human readable syntax.
-- This is the inverse function of the parser,
-- if the format is ignored (e.g. '( +  1 2)` =~ '(+ 1 2)').
readable :: SExpr -> Text
readable (Cons x y) =
  let innerListSyntax = foldl' (<<>>) "" . map readable $ scottDecode y
  in "(" <> readable x <<>> innerListSyntax <> ")"
  where
    a  <<>> "" = a
    "" <<>> b  = b
    a  <<>> b  = a <> " " <> b
readable Nil                         = "()"
readable (AtomSymbol (MaruSymbol x)) = x
readable (AtomInt x)                 = showt x


-- |
-- Concatenate `SExpr` by `Cons`
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

-- | The inverse function of `scottEncode`
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


makePrisms ''SExpr
