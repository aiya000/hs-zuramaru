{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | TODO
module Maru.Eval
  ( Env
  , initialEnv
  , eval
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Lazy (StateT, runStateT)
import Data.Map.Lazy (Map)
import Data.Text (Text)
import Data.Void (Void)
import Maru.Type (SExpr(..))
import qualified Data.Map.Lazy as M

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

data SomeFunc = forall a b. SomeFunc (a -> b)
type Env = Map Text SomeFunc


initialEnv :: Env
initialEnv = M.fromList [ ("+", SomeFunc (+))
                        , ("-", SomeFunc (-))
                        , ("*", SomeFunc (*))
                        , ("/", SomeFunc div)
                        ]


-- | TODO
eval :: SExpr -> IO SExpr
eval = return --void . flip runStateT [] . runMaruEvaluator . execute
