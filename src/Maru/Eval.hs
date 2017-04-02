{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Cons (Symbol "print") (Cons (Symbol "10") Nil)
--
--       Cons
--      /    \
--   print  Cons
--         /    \
--       10    Nil
--
--
-- Cons (Symbol "defun") (Cons (Symbol "first")
--   (Cons (Cons (Symbol "x") (Cons (Symbol "y") Nil))
--     (Cons (Symbol "x") Nil)))
--
--      Cons
--     /    \
--   defun  Cons
--         /    \
--       first  Cons
--             /    \
--           Cons    \
--          /    \    \
--         x    Cons   \
--             /    \   \
--            y     Nil  \
--                      Cons
--                     /    \
--                    x     Nil
--
--
-- Cons (Symbol "print") (Cons (Cons (Symbol "cons") (Cons (Symbol "1") (Cons (Cons (Symbol "cons") (Cons (Symbol "2") (Cons (Cons (Symbol "cons") (Cons (Symbol "3") (Cons (Cons (Symbol "cons") (Cons (Symbol "4") (Cons (Symbol "nil") Nil))) Nil))) Nil)))
--
--       Cons
--      /    \
--   print  Cons
--         /    \
--       Cons  Nil
--      /   \
--    cons  Cons
--         /    \
--        1     Cons
--             /    \
--           cons   Cons
--                 /    \
--               ...    ...

-- | TODO
module Maru.Eval
  ( eval
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Lazy (StateT, runStateT)
import Maru.Eval.Type
import Maru.Eval.Type.ProgramStack
import Maru.Type (SExpr(..))

-- | TODO
newtype SExprEvaluator a = SExprEvaluator
  { runSExprParser :: StateT ProgramStack IO a
  } deriving ( Functor, Applicative, Monad
             , MonadState ProgramStack, MonadIO
             )


-- | TODO
eval :: SExpr -> IO ()
eval = void . flip runStateT [] . runSExprParser . execute


execute :: SExpr -> SExprEvaluator MaruTerm
execute _ = undefined
