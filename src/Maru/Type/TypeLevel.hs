{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Dependent types (Please see 'Maru.QQ')
module Maru.Type.TypeLevel
  ( HighSExpr (..)
  , Sing (..)
  ) where

import Data.Proxy (Proxy(..))
import Data.Singletons (SingI(..), Sing, SingKind(..), SomeSing(..))
import Data.Type.Bool (type (&&))
import Data.Type.Equality (type (==))
import GHC.TypeLits (Symbol, symbolVal, Nat, natVal, KnownSymbol, KnownNat)
import Maru.Type (SExpr(..), MaruSymbol(..))
import qualified Data.Text as T


-- | A dependent type for 'SExpr' (Please see 'Maru.QQ.parse')
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
