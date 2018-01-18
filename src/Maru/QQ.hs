{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Present compile time lisp (zuramaru) literals as quasi-quotes.
--
-- The compile time inline lisp is contained :sunny:
--
-- You can use 'Maru.QQ.ShortName' instead of these functions,
-- if you don't worry name conflictions.
--
-- These requires to
-- `
-- import Maru.Type.SExpr (SExpr(..), MaruSymbol(..))
-- `
-- and add `text` to your package.yaml or {project}.cabal (below is a package.yaml example)
-- `
-- dependencies:
--    - text
-- `
--
-- and optionally requirements are existent, please see below.
--
-- If you want to use as patterns
-- (e.g. `let ![parse|123|] = AtomInt 123`)
-- `
-- {-# LANGUAGE OverloadedStrings #-}
-- `
--
-- If you want to use as types
-- (e.g. `f :: [parse|(1 2)|]`)
-- `
-- {-# LANGUAGE DataKinds #-}
-- `
module Maru.QQ
  ( parse
  , parsePreprocess
  , zurae
  , HighSExpr (..)
  , Sing (..)
  ) where

import Control.Arrow ((>>>))
import Data.Proxy (Proxy(..))
import Data.Singletons (SingI(..), Sing, SingKind(..), SomeSing(..))
import Data.Text (Text)
import Data.Type.Bool (type (&&))
import Data.Type.Equality (type (==))
import GHC.TypeLits (Symbol, symbolVal, Nat, natVal, KnownSymbol, KnownNat)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Maru.Type (SExpr(..), MaruSymbol(..), growUp)
import qualified Data.Text as T
import qualified Maru.Parser as Maru

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XDataKinds
-- >>> import Maru.Type.Parser (ParseErrorResult)
-- >>> import Maru.Type.SExpr (SourceCode, SExpr)
-- >>> import qualified Maru.Parser as Maru
-- >>> import qualified Maru.Preprocessor as Maru

--TODO: Implement tests to check the compiler errors (See 'Maru.QQ.parse', 'Maru.QQ.zurae' documentation for conditions of the compile error)
--      (What can I implement it ?)

-- | A dependent type for 'SExpr' (Please see 'parse')
data HighSExpr = HCons HighSExpr HighSExpr
               | HQuote HighSExpr
               | HAtomInt Nat
               | HAtomBool Bool
               | HAtomSymbol Symbol
               | HNil


data instance Sing (a :: HighSExpr) :: * where
  HAtomSymbolS :: MaruSymbol -> Sing ('HAtomSymbol s)
  HAtomBoolS   :: Bool -> Sing ('HAtomBool x)
  HAtomIntS    :: Int -> Sing ('HAtomInt n)
  HNilS        :: Sing 'HNil
  HQuoteS      :: Sing x -> Sing ('HQuote x)
  HConsS       :: Sing x -> Sing y -> Sing ('HCons x y)

instance SingI 'HNil where
  sing :: Sing 'HNil
  sing = HNilS

instance KnownSymbol s => SingI ('HAtomSymbol s) where
  sing :: Sing ('HAtomSymbol s)
  sing = HAtomSymbolS . MaruSymbol . T.pack $ symbolVal (Proxy :: Proxy s)

instance SingI ('HAtomBool 'True) where
  sing :: Sing ('HAtomBool 'True)
  sing = HAtomBoolS True

instance SingI ('HAtomBool 'False) where
  sing :: Sing ('HAtomBool 'False)
  sing = HAtomBoolS False

instance KnownNat n => SingI ('HAtomInt n) where
  sing :: Sing ('HAtomInt n)
  sing = HAtomIntS . fromInteger $ natVal (Proxy :: Proxy n)

instance SingI x => SingI ('HQuote x) where
  sing :: Sing ('HQuote x)
  sing = let x' = sing :: Sing x
         in HQuoteS x'

instance (SingI x, SingI y) => SingI ('HCons x y) where
  sing :: Sing ('HCons x y)
  sing = let x' = sing :: Sing x
             y' = sing :: Sing y
         in HConsS x' y'


instance SingKind HighSExpr where
  type DemoteRep HighSExpr = SExpr
  fromSing HNilS            = Nil
  fromSing (HAtomSymbolS s) = AtomSymbol s
  fromSing (HAtomBoolS x)   = AtomBool x
  fromSing (HAtomIntS n)    = AtomInt n
  fromSing (HQuoteS x)      = Quote (fromSing x)
  fromSing (HConsS x y)     = Cons (fromSing x) (fromSing y)
  toSing Nil            = SomeSing HNilS
  toSing (AtomSymbol s) = SomeSing $ HAtomSymbolS s
  toSing (AtomBool x)   = SomeSing $ HAtomBoolS x
  toSing (AtomInt n)    = SomeSing $ HAtomIntS n
  toSing (Quote x)      = case toSing x of
                               SomeSing x' -> SomeSing $ HQuoteS x'
  toSing (Cons x y)     = case (toSing x, toSing y) of
                               (SomeSing x', SomeSing y') -> SomeSing $ HConsS x' y'


type instance 'HNil          == 'HNil          = 'True
type instance 'HAtomInt x    == 'HAtomInt y    = x == y
type instance 'HAtomBool x   == 'HAtomBool y   = x == y
type instance 'HAtomSymbol x == 'HAtomSymbol y = x == y
type instance 'HQuote x      == 'HQuote y      = x == y
type instance 'HCons x1 y1   == 'HCons x2 y2   = x1 == x2 && y1 == y2


textE :: Text -> Exp
textE t =
  let x = T.unpack t
  in VarE (mkName "Data.Text.pack") `AppE` LitE (StringL x)

intL :: Int -> Lit
intL = IntegerL . fromIntegral

boolP :: Bool -> Pat
boolP x = ConP (mkName $ show x) []

-- | A value `x :: Int` as a type `x :: 'Nat'`
intT :: Int -> Type
intT = LitT . NumTyLit . fromIntegral

-- | A value `x :: Bool` as a type `x :: 'Bool`
boolT :: Bool -> Type
boolT = PromotedT . mkName . show

-- | A value 'x :: Text' as a type `x :: 'Symbol'`
textSym :: Text -> Type
textSym = LitT . StrTyLit . T.unpack


-- |
-- Expand a code to 'SExpr'
-- if it can be parsed by 'Maru.Parser.parse'
-- (or to be compiled error).
--
-- Occure the compile error if it cannot be parsed.
--
-- >>> :{
-- >>> let p :: SourceCode -> Either ParseErrorResult SExpr
-- >>>     p x = growUp <$> Maru.parse x -- with the empty preprocessor (growUp)
-- >>> :}
--
-- As expressions
--
-- >>> Right [parse|123|] == p "123"
-- True
-- >>> Right [parse|abc|] == p "abc"
-- True
-- >>> Right [parse|(123 456)|] == p "(123 456)"
-- True
--
-- As patterns
--
-- >>> case AtomInt 123 of; [parse|123|] -> "good"
-- "good"
-- >>> case AtomInt 000 of; [parse|123|] -> "bad"; AtomInt _ -> "good"
-- "good"
-- >>> :set -XOverloadedStrings
-- >>> case AtomSymbol "x" of; [parse|x|] -> "good"
-- "good"
-- >>> case Quote (AtomInt 10) of; [parse|'10|] -> "good"
-- "good"
-- >>> case Cons (AtomSymbol "x") (Cons (AtomInt 10) Nil) of; [parse|(x 10)|] -> "good"
-- "good"
--
-- As types (compile time calculations)
--
-- >>> fromSing (sing :: Sing [parse|10|])
-- AtomInt 10
-- >>> fromSing (sing :: Sing [parse|konoko|])
-- AtomSymbol "konoko"
-- >>> fromSing (sing :: Sing [parse|(1 2 3)|])
-- Cons (AtomInt 1) (Cons (AtomInt 2) (Cons (AtomInt 3) Nil))
>>>>>>> d17c276... Implement `parse` quasi-quote
parse :: QuasiQuoter
parse = QuasiQuoter
  { quoteExp = parseToExp
  , quotePat = parseToPat
  , quoteType = parseToType
  , quoteDec = error "Maru.QQ.parse: the expansion to `[Dec]` (`quoteDec`) isn't support supported"
  }
  where
    force :: (SExpr -> a) -> String -> Q a
    force f = T.pack >>> Maru.parse >>> \case
      Left  e -> fail $ Maru.parseErrorPretty e
      Right a -> return . f $ growUp a

    parseToExp :: String -> Q Exp
    parseToExp = force toExp

    toExp :: SExpr -> Exp
    toExp (Cons x y)   = ConE (mkName "Cons") `AppE` ParensE (toExp x) `AppE` ParensE (toExp y)
    toExp (Quote x)    = ConE (mkName "Quote") `AppE` ParensE (toExp x)
    toExp Nil          = ConE (mkName "Nil")
    toExp (AtomInt x)  = ConE (mkName "AtomInt") `AppE` LitE (intL x)
    toExp (AtomBool x) = ConE (mkName "AtomBool") `AppE` ConE (mkName $ show x)
    toExp (AtomSymbol (MaruSymbol x)) = ConE (mkName "AtomSymbol") `AppE`
                                          ParensE (ConE (mkName "MaruSymbol") `AppE` textE x)

    parseToPat :: String -> Q Pat
    parseToPat = force toPat

    toPat :: SExpr -> Pat
    toPat (Cons x y)   = ConP (mkName "Cons") [toPat x, toPat y]
    toPat (Quote x)    = ConP (mkName "Quote") [toPat x]
    toPat Nil          = ConP (mkName "Nil") []
    toPat (AtomInt x)  = ConP (mkName "AtomInt") [LitP $ intL x]
    toPat (AtomBool x) = ConP (mkName "AtomBool") [boolP x]
    -- This requires OverloadedStrings
    toPat (AtomSymbol (MaruSymbol x)) = ConP (mkName "AtomSymbol") [ConP (mkName "MaruSymbol") [LitP . StringL $ T.unpack x]]

    parseToType :: String -> Q Type
    parseToType = force toType

    toType :: SExpr -> Type
    toType (Cons x y)   = PromotedT (mkName "HCons") `AppT` ParensT (toType x) `AppT` ParensT (toType y)
    toType (Quote x)    = PromotedT (mkName "HQuote") `AppT` ParensT (toType x)
    toType Nil          = PromotedT (mkName "HNil")
    --NOTE: Should specify a branch when `x < 0` ? or NumTyLit satisfies ?
    toType (AtomInt x)  = PromotedT (mkName "HAtomInt") `AppT` intT x
    toType (AtomBool x) = PromotedT (mkName "HAtomBool") `AppT` boolT x
    toType (AtomSymbol (MaruSymbol x)) = PromotedT (mkName "HAtomSymbol") `AppT` textSym x


-- |
-- Similar to 'parse' but 'Maru.preprocessor.preprocess' is appended
--
-- >>> :{
-- >>> let pp :: SourceCode -> Either ParseErrorResult SExpr
-- >>>     pp code = Maru.preprocess <$> Maru.parse code
-- >>> :}
--
-- >>> [parsePreprocess|sugar|] == Maru.parse "sugar"
parsePreprocess :: QuasiQuoter
parsePreprocess = undefined


-- |
-- Similar to 'parsePreprocess' but it is ran in the compile time,
-- Occure the side effects,
-- and Return the evaluated result.
--
-- Occure the compile error if the code throws exceptions.
--
-- この関数名zuraeの発音はずら〜（ずらあ）です。
-- (the e suffix means an 'e'valuation)
--
-- >>> [zurae|(print 10)|]
-- 10
-- Nil
-- >>> [zurae|10|]
-- AtomInt 10
-- >>> [zurae|'(1 2 3)|]
-- Cons (Cons (AtomInt 1) (Cons (AtomInt 2) (Cons (AtomInt 3) Nil)))
zurae :: QuasiQuoter
zurae = undefined
