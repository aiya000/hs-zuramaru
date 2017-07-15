{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- If it is a general thing but it is associated only eff,
-- it can be put into here.
module Maru.Type.Eff
  ( ExceptionCause
  , Fail'
  , liftMaybe'
  , nonEmpty'
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.Exception (Exc, liftEither)
import Data.Either.Extra (maybeToEither)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Text (Text)

-- | A message of @Fail'@
type ExceptionCause = Text

-- |
-- An effect of @Eff@.
-- A possible of the failure.
type Fail' = Exc ExceptionCause


-- | Simular to @liftEither@ but from @Maybe a@, take an @ExceptionCause@.
liftMaybe' :: Member Fail' r => ExceptionCause -> Maybe a -> Eff r a
liftMaybe' = (liftEither .) . maybeToEither


-- | Same as @nonEmpty@ but it is lifted to `Member Fail' r => Eff r` context
nonEmpty' :: Member Fail' r => [a] -> Eff r (NonEmpty a)
nonEmpty' = liftMaybe' "An empty list is given" . nonEmpty
