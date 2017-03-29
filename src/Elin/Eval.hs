{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Cons (Symbol "print") (Cons (Symbol "10") Nil)
-- -
-- Cons (Symbol "defun") (Cons (Symbol "first")
--   (Cons (Cons (Symbol "x") (Cons (Symbol "y") Nil))
--     (Cons (Symbol "x") Nil)))

-- | TODO
module Elin.Eval
  ( eval
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (MonadState, gets)
import Control.Monad.State.Lazy (StateT, get, put, runStateT)
import Elin.Eval.Type
import Elin.Eval.Type.ProgramStack
import Elin.Type (SExpr(..))
import qualified Elin.Eval.Type.ProgramStack as PS

-- | TODO
type Something = Int
-- | TODO
type NilType = [Something]

-- | TODO
newtype SExprEvaluator a = SExprEvaluator
  { runSExprParser :: StateT ProgramStack IO a
  } deriving ( Functor, Applicative, Monad
             , MonadState ProgramStack, MonadIO
             )


-- | TODO
eval :: SExpr -> IO ()
eval sexpr = void $ flip runStateT [] . runSExprParser $ execute sexpr


execute :: SExpr -> SExprEvaluator EliValue
execute Nil = return $ EliValue ([] :: NilType)
execute _ = undefined
