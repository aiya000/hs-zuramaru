{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | TODO
module Maru.Eval
  ( eval
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Lazy (StateT, runStateT)
import Data.Text (Text)
import Data.Void (Void)
import Maru.Type (SExpr(..))

-- | TODO
type ProgramStack = Void
-- | TODO
type MaruTerm = Void

-- | A monad for evaluating the program
newtype MaruEvaluator a = MaruEvaluator
  { runMaruEvaluator :: StateT ProgramStack IO a
  } deriving ( Functor, Applicative, Monad
             , MonadState ProgramStack, MonadIO
             )



-- | TODO
eval :: SExpr -> IO SExpr
eval = return --void . flip runStateT [] . runMaruEvaluator . execute
