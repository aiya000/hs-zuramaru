{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
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
  , nonEmpty'
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.Exception (Exc, liftEither)
import Control.Eff.Writer.Lazy (Writer)
import Data.Either.Extra (maybeToEither)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text)
import Maru.Type.SExpr (SExpr, AST(..))
import TextShow (TextShow(..))
import qualified TextShow as TS

-- | A message of @Fail'@
type ExceptionCause = Text

-- |
-- An effect of @Eff@.
-- A possible of the failure.
type Fail' = Exc ExceptionCause


--TODO: Don't define IncoherentInstances, use newtype instead. (1)
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


-- | Same as @nonEmpty@ but it is lifted to `Member Fail' r => Eff r` context
nonEmpty' :: Member Fail' r => [a] -> Eff r (NonEmpty a)
nonEmpty' = liftMaybe' "An empty list is given" . nonEmpty
