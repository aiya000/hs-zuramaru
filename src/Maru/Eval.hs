{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | @MaruEvaluator@ evaluates @SEexpr@.
module Maru.Eval
  ( MaruEnv
  , initialEnv
  , eval
  ) where

import Control.Eff (Eff, Member, SetMember)
import Control.Eff.Exception (Exc, runExc, liftEither)
import Control.Eff.Lift (Lift, runLift)
import Control.Eff.State.Lazy (State, runState, get)
import Control.Exception.Safe (Exception)
import Control.Exception.Throwable.TH (declareException)
import Data.Either.Extra (maybeToEither)
import Data.Map.Lazy (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Typeable (Typeable)
import Maru.Type (SExpr(..), MaruTerm(..))
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

-- Define a data type and instances
declareException "NoSuchSymbolException" ["NoSuchSymbolException"]

-- | This is thrown if looking up symbol is failed in @MaruEvaluator@
type NoSuchSymbolException' = NoSuchSymbolException ()


-- | A monad for evaluating a program
type MaruEvaluator a = forall r. ( Member (Exc NoSuchSymbolException') r
                                 , Member (State MaruEnv) r
                                 , SetMember Lift (Lift IO) r
                                 ) => Eff r a

--NOTE: Why eff's runState's type sigunature is different with mtl runState ?
-- | Run an evaluation of @MaruEvaluator@
runMaruEvaluator :: MaruEvaluator a -> MaruEnv -> IO (Either NoSuchSymbolException' a, MaruEnv)
runMaruEvaluator m env = swap <$> (runLift . runState env $ runExc m)


-- |
-- Take a value from @MaruEnv@ in @State@.
-- If @sym@ is not exists, take invalid value of @Exc NoSuchSymbolException'@
lookupSymbol :: forall r. (Member (Exc NoSuchSymbolException') r, Member (State MaruEnv) r) => Text -> Eff r SomeFunc
lookupSymbol sym = do
  env <- (get :: Eff r MaruEnv)
  liftEither . maybeToEither (symbolNotFound sym) $ M.lookup sym env
  where
    symbolNotFound :: Text -> NoSuchSymbolException'
    symbolNotFound sym = noSuchSymbolException $ "Symbol '" <> T.unpack sym <> "' is not found"


type MaruEnv = Map Text SomeFunc

data SomeFunc = forall a b. SomeFunc (a -> b)


-- |
-- An initial value of the runtime of evaluation.
--
-- This is a state of @MaruEvaluator@.
--
-- This maybe passed to @eval@
initialEnv :: MaruEnv
initialEnv = M.fromList [ ("+", SomeFunc ((+) :: Int -> Int -> Int))
                        , ("-", SomeFunc ((-) :: Int -> Int -> Int))
                        , ("*", SomeFunc ((*) :: Int -> Int -> Int))
                        , ("/", SomeFunc (div :: Int -> Int -> Int))
                        ]


-- |
-- Evaluate a S expression,
-- and happen its side effects.
--
-- If you don't have a value of @MaruEnv@, you can use @initialEnv@.
--
-- Return an evaluated result, with new @MaruEnv@
-- (@env@ is changed if the evaluation of @SExpr@ changes @MaruEnv@).
eval :: MaruEnv -> SExpr -> IO (SExpr, MaruEnv)
eval env sexpr = do
  (result, newEnv) <- runMaruEvaluator (execute sexpr) env
  case result of
    Left  e -> print e >> return (Nil, newEnv)
    Right a -> return (a, newEnv)


-- | A naked evaluator of zuramaru.
execute :: SExpr -> MaruEvaluator SExpr
execute Nil       = return Nil
execute (Quote _) = error "TODO (eval)"
execute (Cons _ _) = undefined
execute (Atom (TermInt x)) = undefined --return x
execute (Atom (TermSymbol symbol)) = do
  lookupSymbol symbol
  undefined
