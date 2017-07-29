{-# LANGUAGE Rank2Types #-}

-- | For general things of Lens
module Maru.Type.Lens
  ( (^$?)
  ) where

import Control.Lens ((^?), Getting)
import Data.Monoid (First)


infixl 8 ^$?

--NOTE: Can this is alternated by something ? https://www.stackage.org/haddock/lts-8.11/lens-4.15.1/Control-Lens-Fold.html#v:-94--63-
-- | (^?) over a `Functor`
(^$?) :: Functor f => f s -> Getting (First a) s a -> f (Maybe a)
fb ^$? acs = (^? acs) <$> fb
