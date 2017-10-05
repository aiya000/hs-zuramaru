{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Integrate types of extensible's Effect.
--
-- `MaruMacro` is evaluated by `MaruEvaluator`.
-- `MaruFunc` is calculated by `MaruCalculator`.
module Maru.Type.Eval
  ( Fail
  , FailKey
  , FailValue
  , FailAssociation
  , throwFail
  , includeFail
  , SimplificationSteps
  , reportSteps
  , SimplifSteps
  , SimplifStepsKey
  , SimplifStepsValue
  , SimplifStepsAssociation
  , MaruScopes
  , MaruScopesKey
  , MaruScopesValue
  , MaruScopesAssociation
  , insertGlobalVar
  , newScope
  , popNewerScope
  , MaruEnv
  , getMaruEnv
  , putMaruEnv
  , modifyMaruEnv
  , IOEff
  , IOEffKey
  , IOEffValue
  , IOEffAssociation
  , liftIOEff
  , ExceptionCause
  , MaruEvaluator
  , runMaruEvaluator
  , newSymbol
  , MaruScope
  , MaruFunc (..)
  , MaruMacro (..)
  , lookup
  , lookupVar
  , (^$)
  , MaruCalculator
  , runMaruCalculator
  , First' (..)
  , first'
  ) where

import Control.Lens hiding ((<|))
import Control.Monad.Fail (MonadFail(..))
import Data.Extensible
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Map.Lazy (Map)
import Data.Monoid (First(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Data.Typeable (Typeable, typeRep)
import Data.Unique (newUnique, hashUnique)
import Maru.Type.SExpr
import Prelude hiding (fail, lookup)
import TextShow (TextShow(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified Maru.Type.SExpr as MTS

-- $setup
-- >>> import Data.Either

-- | A message of @Fail@
type ExceptionCause = Text

-- |
-- An effect of @MaruEvaluator@.
-- A possible of the failure.
type Fail      = FailKey >: FailValue
type FailKey   = "fail"
type FailValue = EitherEff ExceptionCause
-- | `Fail`'s `Associate`
type FailAssociation = Associate FailKey FailValue

-- | `throwEff` for `Fail`
throwFail :: FailAssociation xs => ExceptionCause -> Eff xs a
throwFail = throwEff #fail

-- |
-- Include `Maybe` to `Fail` context.
-- If it is Nothing,
-- the whole of `Fail a` to be failed.
includeFail :: FailAssociation xs => ExceptionCause -> Eff xs (Maybe a) -> Eff xs a
includeFail cause mm = do
  maybeIt <- mm
  case maybeIt of
    Nothing -> throwFail cause
    Just x  -> return x


-- | A log for 簡約s
type SimplificationSteps = [SExpr]

-- |
-- Append numbers to steps
--
-- >>> import Maru.Type.SExpr
-- >>> reportSteps [Cons (AtomInt 1) (Cons (AtomInt 2) Nil), Cons (AtomInt 2) Nil]
-- ["1: (1 2)","2: (2)"]
reportSteps :: SimplificationSteps -> [Text]
reportSteps = zipWith appendStepNumber [1..] . map readable
  where
    appendStepNumber :: Int -> Text -> Text
    appendStepNumber n x = showt n <> ": " <> x

-- | An effect of @MaruEvaluator@, for logging simplifications
type SimplifSteps      = SimplifStepsKey >: SimplifStepsValue
type SimplifStepsKey   = "simplifSteps"
type SimplifStepsValue = WriterEff SimplificationSteps
type SimplifStepsAssociation = Associate SimplifStepsKey SimplifStepsValue


-- |
-- An effect of @MaruEvaluator@.
-- This is a stack for the lexical scope.
type MaruScopes      = MaruScopesKey >: MaruScopesValue
type MaruScopesKey   = "maruScopes"
type MaruScopesValue = State MaruEnv
type MaruScopesAssociation = Associate MaruScopesKey MaruScopesValue

-- |
-- The whole of the runtime state.
-- This is `NonEmpty`, because the global scope is defined with the program startup
--
-- and
--
-- 'getMaruEnv >>= return . last' is the toplevel (and this is used as global scope).
-- 'getMaruEnv >>= return . head' is the newest scope.
--
-- ( This means the cons operation makes a new scope, it is not '++ [x]'.
--   Please see `makeScope`.
-- )
type MaruEnv = NonEmpty MaruScope

-- | Insert a variable to the toplevel scope
insertGlobalVar :: MaruScopesAssociation xs => MaruSymbol -> SExpr -> Eff xs ()
insertGlobalVar sym val = do
  env <- getMaruEnv
  let env'   = NE.init env
      global = M.insert sym val $ NE.last env
  putMaruEnv $ case NE.nonEmpty env' of
    Nothing    -> (global :| [])
    Just env'' -> env'' <> [global]

-- |
-- Make a scope in the state, with a variable.
--
-- NOTE:
-- The scope must be created with one or more variables.
-- It keeps any safety.
--
-- e.g.
--   1. unintended empty scope is never created
--   2. high affinity of `MaruEnv` (`NonEmpty`)  is kept
newScope :: MaruScopesAssociation xs => MaruSymbol -> SExpr -> Eff xs ()
newScope sym val = modifyMaruEnv ([(sym, val)] <|)

-- |
-- Remove the newest scope (about the newest scope is written in `MaruEnv`),
-- and Return removed scope
popNewerScope :: MaruScopesAssociation xs => Eff xs MaruScope
popNewerScope = do
  (newest:|restEnv) <- getMaruEnv
  case NE.nonEmpty restEnv of
    Nothing       -> error "localScope(fatal error!): unexpected empty stack is detected! (In the correct case, the global scope is existed)"
    Just restEnv' -> putMaruEnv restEnv' >> return newest

-- |
-- The runtime state.
-- This associates the variable name and the value.
type MaruScope = Map MaruSymbol SExpr

-- | `getEff` for `MaruScopes`
getMaruEnv :: MaruScopesAssociation xs => Eff xs MaruEnv
getMaruEnv = getEff #maruScopes

-- | `putEff` for `MaruScopes`
putMaruEnv :: MaruScopesAssociation xs => MaruEnv -> Eff xs ()
putMaruEnv = putEff #maruScopes

-- | `modifyEff` for `maruScopes`
modifyMaruEnv :: MaruScopesAssociation xs => (MaruEnv -> MaruEnv) -> Eff xs ()
modifyMaruEnv = modifyEff #maruScopes

-- | Find a variable from the whole of the runtime environment with a symbol
lookup :: MaruSymbol -> MaruEnv -> Maybe SExpr
lookup sym xs = getFirst . mconcat . NE.toList $ NE.map (First . M.lookup sym) xs


-- | An effect of @MaruEvaluator@, this is same as `IO` in `Eff`
type IOEff      = IOEffKey >: IOEffValue
type IOEffKey   = "ioEff"
type IOEffValue = IO
type IOEffAssociation = Associate IOEffKey IOEffValue

-- | `liftEff` for `IOEff`
liftIOEff :: IOEffAssociation xs => IO a -> Eff xs a
liftIOEff = liftEff #ioEff


-- | A monad for evaluating a maru's program
type MaruEvaluator = Eff '[Fail, MaruScopes, SimplifSteps, IOEff]

instance MonadFail MaruEvaluator where
  fail = throwFail . T.pack


--NOTE: Why eff's runState's type sigunature is different with mtl runState ?
-- | Run an evaluation of @MaruEvaluator a@
runMaruEvaluator :: MaruEvaluator a -> MaruEnv -> IO (Either ExceptionCause a, MaruEnv, SimplificationSteps)
runMaruEvaluator m env = flatten <$> runMaruEvaluator' m env
  where
    runMaruEvaluator' :: MaruEvaluator a -> MaruEnv -> IO (((Either ExceptionCause a), MaruEnv), [SExpr])
    runMaruEvaluator' m env = retractEff . runWriterEff . flip runStateEff env $ runEitherEff m
    flatten :: ((a, b), c) -> (a, b, c)
    flatten ((x, y), z) = (x, y, z)

-- |
-- Create a symbol of the variable name,
-- it is unique (is not duplicated) in the runtime.
newSymbol :: MaruEvaluator MaruSymbol
newSymbol = ("n" <>) <$> newSymbol' ""
  where
    -- Take a base of the name.
    newSymbol' :: MaruSymbol -> MaruEvaluator MaruSymbol
    newSymbol' baseName = do
      name <- liftIOEff $ (baseName <>) . MTS.pack . show . hashUnique <$> newUnique
      env <- getMaruEnv
      case lookup name env of
           Nothing -> newSymbol' name
           Just  _ -> return name

--NOTE: Can this is alternated by some lens's function ?
-- |
-- This is like `Prism`'s accessor,
-- but don't return result as `Maybe`.
--
-- Similar to 'x <&> review f' but Nothing is included as a failure of the whole of `MaruEvaluator`.
--
-- `Typeable` for the error message.
(^$) :: (Typeable s, Typeable a) => MaruEvaluator s -> Getting (First a) s a -> MaruEvaluator a
(m :: MaruEvaluator s) ^$ (acs :: Getting (First a) s a) = do
  let typeNameOfS = T.pack . show $ typeRep (Proxy :: Proxy s)
      typeNameOfA = T.pack . show $ typeRep (Proxy :: Proxy a)
      cause = "(^$): `" <> typeNameOfA <> "` couldn't be getten from `" <> typeNameOfS <> "`"
  includeFail cause $ m <&> preview acs


-- |
-- This has effects of the part of `MaruEvaluator`.
-- Calculate pure functions.
type MaruCalculator = Eff '[Fail]

instance MonadFail MaruCalculator where
  fail = throwFail . T.pack

-- | Extract the pure calculation from `MaruCalculator`
runMaruCalculator :: MaruCalculator a -> Either ExceptionCause a
runMaruCalculator = leaveEff . runEitherEff


-- | Simular to `First`, but using '`Either` `ExceptionCause`' instead of `Maybe`
newtype First' a = First'
  { getFirst' :: Either ExceptionCause a
  } deriving (Functor)

instance Monoid (First' a) where
  mempty = First' $ Left "mempty: `First'` couldn't find the right value"
  w@(First' (Right _)) `mappend` _ = w
  _ `mappend` w@(First' (Right _)) = w
  _ `mappend` _                    = mempty


-- |
-- Like a consturctor, but from '`Maybe` a'.
-- If `Nothing` is given, return `mempty`.
first' :: Maybe a -> First' a
first' (Just a) = First' $ Right a
first' Nothing  = mempty


-- |
-- A function of maru.
-- This keeps the purity, don't happen effects.
--
-- Take [`SExpr`] as arguments, its length is checked by each function.
-- If it is not the expected length, `Nothing` maybe given.
--
-- Notice:
--
-- The function is Haskell's function, is represented by Haskell.
-- The function is not maru's (runtime's) function (cannot be defined in the runtime).
newtype MaruFunc = MaruFunc
  { execFunc :: [SExpr] -> MaruCalculator SExpr
  }

-- |
-- A macro of maru,
-- this means the impure function.
--
-- Similar to `MaruFunc`,
-- but this is possibility to update the state of the environment.
newtype MaruMacro = MaruMacro
  { execMacro :: SExpr -> MaruEvaluator SExpr
  }


-- |
-- Take a variable from `MaruScopes` effect.
-- If `sym` is not exists, the whole of this `Eff` to be failed
lookupVar :: forall xs.
             ( FailAssociation xs
             , MaruScopesAssociation xs
             ) => MaruSymbol -> Eff xs SExpr
lookupVar sym = do
  env <- getMaruEnv
  let cause = "A symbol '" <> unMaruSymbol sym <> "' is not found"
  includeFail cause . return $ lookup sym env
