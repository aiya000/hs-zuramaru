{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | @MaruEvaluator@ evaluates @SEexpr@.
module Maru.Eval
  ( initialEnv
  , eval
  ) where

import Control.Eff.Exception (throwExc)
import Control.Exception.Safe (Exception, SomeException, toException)
import Control.Exception.Throwable.TH (declareException)
import Control.Monad (foldM)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Maru.Type (SExpr(..), scottDecode, nonEmpty')
import Maru.Type.Eval
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

declareException "EvalException" ["EvalException"]


-- |
-- An initial value of the runtime of evaluation.
--
-- This is a state of @MaruEvaluator@.
--
-- This maybe passed to @eval@
initialEnv :: MaruEnv
initialEnv = M.fromList [ ("+", SomeMaruPrimitive DiscrIntXIntToInt (+))
                        , ("-", SomeMaruPrimitive DiscrIntXIntToInt (-))
                        , ("*", SomeMaruPrimitive DiscrIntXIntToInt (*))
                        , ("/", SomeMaruPrimitive DiscrIntXIntToInt div)
                        ]


-- |
-- Evaluate a S expression,
-- and happen its side effects.
--
-- If you don't have a value of @MaruEnv@, you can use @initialEnv@.
--
-- Return an evaluated result, with new @MaruEnv@
-- (@env@ is changed if the evaluation of @SExpr@ changes @MaruEnv@).
eval :: MaruEnv -> SExpr -> IO (Either SomeException (SExpr, MaruEnv))
eval env sexpr = do
  (result, newEnv) <- runMaruEvaluator (execute sexpr) env
  case result of
    Left cause  -> return . Left . toException $ EvalException (T.unpack cause) sexpr
    Right sexpr -> return $ Right (sexpr, newEnv)


-- | A naked evaluator of zuramaru
execute :: SExpr -> MaruEvaluator SExpr
execute (Cons (AtomSymbol x) xs) = do
  SomeMaruPrimitive DiscrIntXIntToInt f <- lookupSymbol' x
  -- Evaluate recursively
  xs' <- (nonEmpty'' =<<) . mapM execute $ flatten xs
  foldM1 (liftBinaryFunc f) xs'
  where
    lookupSymbol' :: Text -> MaruEvaluator SomeMaruPrimitive
    lookupSymbol' = lookupSymbol
    nonEmpty'' :: [SExpr] -> MaruEvaluator (NonEmpty SExpr)
    nonEmpty'' = nonEmpty'

execute (Cons (AtomInt x) Nil) = return $ AtomInt x
execute (Cons x y)             = return $ Cons x y
execute (AtomSymbol symbol)    = throwExc ("An operator (" <> symbol <> ") is specified without any argument" :: ExceptionCause)
execute (AtomInt x)            = return $ AtomInt x
execute Nil                    = return Nil
execute (Quote _)              = error "TODO (eval)"


-- |
-- >>> let x = Cons (AtomInt 1) (Cons (AtomInt 2) (Cons (AtomInt 3) Nil)) -- (1 2 3)
-- >>> flatten x
-- [AtomInt 1, AtomInt 2, AtomInt 3]
-- >>> let y = Cons (AtomInt 2) (Cons (AtomSymbol "*") (Cons (AtomInt 3) (Cons (AtomInt 4) Nil))) -- (2 (* 3 4))
-- >>> flatten y
-- [AtomInt 2, Cons (AtomSymbol "*") (Cons (AtomInt 3) (Cons (AtomInt 4) Nil))]
-- >> let z = Cons (AtomSymbol "*") (Cons (AtomInt 3) (Cons (AtomInt 4) Nil)) -- (* 3 4)
-- >> flatten z
-- [Cons (AtomSymbol "*") (Cons (AtomInt 3) (Cons (AtomInt 4) Nil))]
flatten :: SExpr -> [SExpr]
flatten (Cons (AtomInt x) y)      = AtomInt x : flatten y
flatten s@(Cons (AtomSymbol _) _) = [s]
flatten (Cons _ _) = error "an unexpected case is detected (flatten)"
flatten Nil        = []
flatten s@(AtomInt _)    = [s]
flatten s@(AtomSymbol _) = [s]
flatten (Quote _)        = error "TODO (flatten)"


-- | Simular to @foldM@ but for @NonEmpty@
foldM1 :: Monad m => (a -> a -> m a) -> NonEmpty a -> m a
foldM1 f (x :| xs) = foldM f x xs
