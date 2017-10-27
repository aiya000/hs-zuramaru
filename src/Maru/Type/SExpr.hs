{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}


-- | Common types for zuramaru
module Maru.Type.SExpr
  ( SourceCode
  , MaruToken
  , CallowSExpr(..)
  , pattern Cons'
  , pattern Nil'
  , pattern AtomInt'
  , pattern AtomBool'
  , pattern AtomSymbol'
  , pattern Quote'
  , SExpr(..)
  , isAtomInt
  , unAtomInt
  , isAtomSymbol
  , unAtomSymbol
  , SExprLike(..)
  , readable
  , MaruSymbol(..)
  , pack
  , unpack
  , asSymbolList
  , scottEncode
  , scottDecode
  , scottEncode'
  , _Cons
  , _Nil
  , _AtomInt
  , _AtomSymbol
  , SExprIntBullet(..)
  , intBullet
  ) where

import Control.Lens hiding (_Cons)
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
-- >>> import Control.Lens ((^?))
-- >>> import Maru.Parser (parse)
-- >>> import Maru.Preprocessor (preprocess)

-- |
-- The format for the code of maru.
-- (This doesn't mean a file path of the code.)
type SourceCode = Text

-- | The format for the token of `MaruParser`
type MaruToken = P.Token Text


-- |
-- Never preprocessed ('Maru.Preprocessor.preprocess') 'SExpr',
-- this is used only between the parser and the preprocessor.
--
-- This should not be used in the evaluator,
-- and use 'SExpr' instead of this in the evaluator.
--
-- This is simply isomorphic with 'SExpr', please see 'SExpr' the about of this.
data CallowSExpr = CallowSExpr { growUp :: SExpr }
  deriving (Show, Eq)

pattern Cons' :: CallowSExpr -> CallowSExpr -> CallowSExpr
pattern Cons' x y <- CallowSExpr (Cons (CallowSExpr -> x) (CallowSExpr -> y))
  where
    Cons' x y = CallowSExpr (Cons (growUp x) (growUp y))

pattern Nil' :: CallowSExpr
pattern Nil' = CallowSExpr Nil

pattern AtomInt' :: Int -> CallowSExpr
pattern AtomInt' x = CallowSExpr (AtomInt x)

pattern AtomBool' :: Bool -> CallowSExpr
pattern AtomBool' x = CallowSExpr (AtomBool x)

pattern AtomSymbol' :: MaruSymbol -> CallowSExpr
pattern AtomSymbol' x = CallowSExpr (AtomSymbol x)

pattern Quote' :: CallowSExpr -> CallowSExpr
pattern Quote' x <- x
  where
    Quote' (CallowSExpr x) = CallowSExpr (Quote x)

instance TextShow CallowSExpr where
  showb = showb . growUp


-- | n-ary tree and terms
data SExpr = Cons SExpr SExpr -- ^ Appending list and list
           | Nil              -- ^ A representation of empty list
           | AtomInt Int      -- ^ A pattern of the atom for `Int`
           | AtomBool Bool    -- ^ A pattern of the atom for `Bool`
           | AtomSymbol MaruSymbol -- ^ A pattern of the atom for `MaruSymbol`
           | Quote SExpr -- ^ Delays the evaluation of a 'SExpr'
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

-- | Similar to `unAtomInt`
unAtomSymbol :: SExpr -> Maybe MaruSymbol
unAtomSymbol (AtomSymbol x) = Just x
unAtomSymbol _              = Nothing

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
  deriving (IsString, Semigroup, Monoid, Eq, Ord)

--TODO: `show x` should be `"MaruSymbol " ++ show (unpack x)`
instance Show MaruSymbol where
  show x = show $ unpack x

instance TextShow MaruSymbol where
  showb = TS.fromString . show

-- |
-- Wrap `String`.
-- If you want to wrap `Text`, please use `MaruSymbol` value constructor instead.
pack :: String -> MaruSymbol
pack = MaruSymbol . T.pack

-- | A dual of `pack`
unpack :: MaruSymbol -> String
unpack = T.unpack . unMaruSymbol

-- |
-- A `Prism` accessor.
--
-- Get `Nothing` if [`SExpr`] includes non `AtomSymbol`.
-- Get all `AtomSymbol` otherwise.
--
-- >>> [AtomSymbol "x", AtomSymbol "y"] ^? asSymbolList
-- Just ["x","y"]
--
-- >>> [AtomInt 1, AtomSymbol "y"] ^? asSymbolList
-- Nothing
asSymbolList :: Prism' [SExpr] [MaruSymbol]
asSymbolList = prism from to
  where
    from :: [MaruSymbol] -> [SExpr]
    from = map AtomSymbol
    to :: [SExpr] -> Either [SExpr] [MaruSymbol]
    to xs = case (filter (not . isAtomSymbol) xs, mapM unAtomSymbol xs) of
                 ([], Just xs') -> Right xs'
                 (_, _)         -> Left xs


--TODO: this maybe not needed
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
-- Show 'SExpr' as the human readable syntax.
-- This is the inverse function of the parser,
-- if the format is ignored (e.g. '( +  1 2)` =~ '(+ 1 2)').
--
-- vvv invertibilities for 'SExpr' vvv
--
-- >>> readable . preprocess <$> parse "10"
-- Right "10"
-- >>> (preprocess <$>) . parse . readable $ AtomInt 10
-- Right (AtomInt 10)
--
-- >>> readable . preprocess <$> parse "true"
-- Right "true"
-- >>> (preprocess <$>) . parse . readable $ AtomBool True
-- Right (AtomBool True)
--
-- >>> readable . preprocess <$> parse "(+ 1 2)"
-- Right "(+ 1 2)"
-- >>> let result = (preprocess <$>) . parse . readable $ Cons (AtomSymbol "+") (Cons (AtomInt 1) (Cons (AtomInt 2) Nil))
-- >>> result == Right (Cons (AtomSymbol "+") (Cons (AtomInt 1) (Cons (AtomInt 2) Nil)))
-- True
readable :: SExpr -> Text
readable (Cons x y) =
  let innerListSyntax = foldl' (<<>>) "" . map readable $ scottDecode y
  in "(" <> readable x <<>> innerListSyntax <> ")"
  where
    a  <<>> "" = a
    "" <<>> b  = b
    a  <<>> b  = a <> " " <> b
readable Nil = "()"
readable (AtomSymbol (MaruSymbol x)) = x
readable (AtomInt x) = showt x
readable (AtomBool True) = "true"
readable (AtomBool False) = "false"
readable (Quote x) = "(quote " <> showt x <> ")"


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

-- |
-- The inverse function of `scottEncode`
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
scottDecode (AtomInt x) = [AtomInt x]
scottDecode (AtomBool x) = [AtomBool x]
scottDecode (Quote x) = [Quote x]

-- | Same as 'scottEncode' but for 'CallowSExpr'
scottEncode' :: [CallowSExpr] -> CallowSExpr
scottEncode' = dimap (map growUp) CallowSExpr scottEncode


makePrisms ''SExpr
