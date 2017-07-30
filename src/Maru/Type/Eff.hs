{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- If it is a general thing but it is associated only eff,
-- it can be put into here.
module Maru.Type.Eff
  ( ExceptionCause
  , Fail'
  , SimplificationSteps
  , reportSteps
  , WriterSimplifSteps
  , liftMaybe'
  , liftMaybeM
  , nonEmpty'
  ) where

import Control.Eff (Eff, Member, SetMember)
import Control.Eff.Exception (Exc, liftEither, Fail, liftMaybe)
import Control.Eff.Lift (Lift, lift)
import Control.Eff.Writer.Lazy (Writer)
import Data.Either.Extra (maybeToEither)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Maru.Type.SExpr (SExpr, AST(..))
import TextShow (TextShow(..))

-- | A message of @Fail'@
type ExceptionCause = Text

-- |
-- An effect of @Eff@.
-- A possible of the failure.
type Fail' = Exc ExceptionCause


-- A log for 簡約s
type SimplificationSteps = [SExpr]

-- |
-- Append numbers to steps
--
-- >>> import Maru.Type.SExpr
-- >>> reportSteps [Cons (AtomInt 1) (Cons (AtomInt 2) Nil), Cons (AtomInt 2) Nil]
-- ["1: (1 2)","2: (2)"]
reportSteps :: SimplificationSteps -> [Text]
reportSteps = zipWith appendStepNumber [1..] . map visualize
  where
    appendStepNumber :: Int -> Text -> Text
    appendStepNumber n x = showt n <> ": " <> x


-- |
-- An effect of @Eff@,
-- for logging simplifications
type WriterSimplifSteps = Writer SimplificationSteps


-- | Simular to @liftEither@ but from @Maybe a@, take an @ExceptionCause@.
liftMaybe' :: Member Fail' r => ExceptionCause -> Maybe a -> Eff r a
liftMaybe' = (liftEither .) . maybeToEither

--TODO: Remove this after @liftMaybeM@ is added to extensible-effects by my contribute !
-- | Why @liftMaybeM@ is not defined in @Control.Eff.Exception@ ?
liftMaybeM :: ( Typeable m
              , Member Fail r, SetMember Lift (Lift m) r
              ) => m (Maybe a) -> Eff r a
liftMaybeM m = lift m >>= liftMaybe


-- | Same as @nonEmpty@ but it is lifted to `Member Fail' r => Eff r` context
nonEmpty' :: Member Fail' r => [a] -> Eff r (NonEmpty a)
nonEmpty' = liftMaybe' "An empty list is given" . nonEmpty
