{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Integrate types of extensible's Effect.
--
-- `MaruMacro` is evaluated by `MaruEvaluator`.
-- `MaruFunc` is calculated by `MaruCalculator`.
module Maru.Type.Eval
  ( Fail
  , FailKey
  , FailValue
  , throwFail
  , includeFail
  , SimplificationSteps
  , reportSteps
  , SimplifSteps
  , SimplifStepsKey
  , SimplifStepsValue
  , MaruVariables
  , MaruVariablesKey
  , MaruVariablesValue
  , getMaruEnv
  , putMaruEnv
  , modifyMaruEnv
  , IOEff
  , IOEffKey
  , IOEffValue
  , liftIOEff
  , ExceptionCause
  , MaruEvaluator
  , runMaruEvaluator
  , Discriminating (..)
  , MaruEnv
  , MaruFunc
  , MaruMacro
  , SomeMaruPrimitive (..)
  , MaruPrimitive (..)
  , lookupSymbol
  , _SomeMaruPrimitive
  , (^$)
  , MaruCalculator
  , runMaruCalculator
  , First' (..)
  , first'
  ) where

import Control.Lens
import Control.Monad.Fail (MonadFail(..))
import Data.Extensible
import Data.Map.Lazy (Map)
import Data.Monoid ((<>), First)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable (Typeable, typeRep)
import Maru.Type.SExpr
import Prelude hiding (fail)
import TextShow (TextShow(..))
import qualified Data.Map.Lazy as M
import qualified Data.Text as T


-- | A message of @Fail@
type ExceptionCause = Text

-- |
-- An effect of @MaruEvaluator@.
-- A possible of the failure.
type Fail      = FailKey >: FailValue
type FailKey   = "fail"
type FailValue = EitherEff ExceptionCause

-- | `throwEff` for `Fail`
throwFail :: Associate FailKey FailValue xs => ExceptionCause -> Eff xs a
throwFail = throwEff #fail

-- |
-- Include `Maybe` to `Fail` context.
-- If it is Nothing,
-- the whole of `Fail a` to be failed.
includeFail :: Associate FailKey FailValue xs => ExceptionCause -> Eff xs (Maybe a) -> Eff xs a
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
reportSteps = zipWith appendStepNumber [1..] . map visualize
  where
    appendStepNumber :: Int -> Text -> Text
    appendStepNumber n x = showt n <> ": " <> x

-- | An effect of @MaruEvaluator@, for logging simplifications
type SimplifSteps      = SimplifStepsKey >: SimplifStepsValue
type SimplifStepsKey   = "simplifSteps"
type SimplifStepsValue = WriterEff SimplificationSteps


-- | An effect of @MaruEvaluator@, for runtime states.
type MaruVariables      = MaruVariablesKey >: MaruVariablesValue
type MaruVariablesKey   = "maruVariables"
type MaruVariablesValue = State MaruEnv

-- | `getEff` for `MaruVariables`
getMaruEnv :: Associate MaruVariablesKey MaruVariablesValue xs => Eff xs MaruEnv
getMaruEnv = getEff #maruVariables

-- | `putEff` for `MaruVariables`
putMaruEnv :: Associate MaruVariablesKey MaruVariablesValue xs => MaruEnv -> Eff xs ()
putMaruEnv = putEff #maruVariables

-- | `modifyEff` for `maruVariables`
modifyMaruEnv :: Associate MaruVariablesKey MaruVariablesValue xs => (MaruEnv -> MaruEnv) -> Eff xs ()
modifyMaruEnv = modifyEff #maruVariables


-- | An effect of @MaruEvaluator@, this is same as `IO` in `Eff`
type IOEff      = IOEffKey >: IOEffValue
type IOEffKey   = "ioEff"
type IOEffValue = IO

-- | `liftEff` for `IOEff`
liftIOEff :: Associate IOEffKey IOEffValue xs => IO a -> Eff xs a
liftIOEff = liftEff #ioEff


-- | A monad for evaluating a maru's program
type MaruEvaluator = Eff '[Fail, MaruVariables, SimplifSteps, IOEff]

instance MonadFail MaruEvaluator where
  fail = throwFail . T.pack


--TODO: Can Fail context include SimplifSteps context ? (I want to catch a canceled logs for debugging)
--NOTE: Why eff's runState's type sigunature is different with mtl runState ?
-- | Run an evaluation of @MaruEvaluator a@
runMaruEvaluator :: MaruEvaluator a -> MaruEnv -> IO (Either ExceptionCause a, MaruEnv, SimplificationSteps)
runMaruEvaluator m env = flatten <$> runMaruEvaluator' m env
  where
    runMaruEvaluator' :: MaruEvaluator a -> MaruEnv -> IO (((Either ExceptionCause a), MaruEnv), [SExpr])
    runMaruEvaluator' m env = retractEff . runWriterEff . flip runStateEff env $ runEitherEff m
    flatten :: ((a, b), c) -> (a, b, c)
    flatten ((x, y), z) = (x, y, z)


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
type MaruFunc = [SExpr] -> MaruCalculator SExpr

-- |
-- A macro of maru,
-- this means the impure function.
--
-- Similar to `MaruFunc`,
-- but this is possibility to update the state of the environment.
type MaruMacro = [SExpr] -> MaruEvaluator SExpr


-- | An identifier for dicriminate a type of @SomeMaruPrimitive@
data Discriminating :: * -> * where
  -- | The identifier for a normalized term of the integer
  DiscrInt  :: Discriminating Int
  -- | The identifier for a variable of maru
  DiscrText :: Discriminating Text
  -- | The identifier for a function
  DiscrFunc :: Discriminating MaruFunc
  -- | The identifier for a macro
  DiscrMacro :: Discriminating MaruMacro


-- | The state of the runtime
type MaruEnv = Map MaruSymbol SomeMaruPrimitive

-- | A reversible monomorphic type for @MaruPrimitive@
data SomeMaruPrimitive = forall a. MaruPrimitive a => SomeMaruPrimitive (Discriminating a) a

-- | A `Prism` for `SomeMaruPrimitive`
_SomeMaruPrimitive :: MaruPrimitive a => Discriminating a -> Prism' SomeMaruPrimitive a
_SomeMaruPrimitive DiscrInt = prism' (SomeMaruPrimitive DiscrInt) $
  \case SomeMaruPrimitive DiscrInt x -> Just x
        _ -> Nothing
_SomeMaruPrimitive DiscrText = prism' (SomeMaruPrimitive DiscrText) $
  \case SomeMaruPrimitive DiscrText x -> Just x
        _ -> Nothing
_SomeMaruPrimitive DiscrFunc = prism' (SomeMaruPrimitive DiscrFunc) $
  \case SomeMaruPrimitive DiscrFunc f -> Just f
        _ -> Nothing
_SomeMaruPrimitive DiscrMacro = prism' (SomeMaruPrimitive DiscrMacro) $
  \case SomeMaruPrimitive DiscrMacro f -> Just f
        _ -> Nothing


instance Show SomeMaruPrimitive where
  show x = "SomeMaruPrimitive " ++ case x of
    SomeMaruPrimitive DiscrInt  a -> show a
    SomeMaruPrimitive DiscrText a -> T.unpack a
    SomeMaruPrimitive DiscrFunc  _ -> "#func"
    SomeMaruPrimitive DiscrMacro _ -> "#macro"


-- |
-- A value of the runtime.
-- These can be used in the code of maru.
--
-- This is strongly associated with @MaruTerm@.
class MaruPrimitive a
instance MaruPrimitive Int -- ^ integral terms
instance MaruPrimitive Text
instance MaruPrimitive MaruFunc -- ^ maru functions
instance MaruPrimitive MaruMacro -- ^ maru macros


-- |
-- Take a value from `MaruEnv` in `State`.
-- If `sym` is not exists, take invalid value of '`Exc` `NoSuchSymbolException'`'
lookupSymbol :: forall xs.
                ( Associate FailKey FailValue xs
                , Associate MaruVariablesKey MaruVariablesValue xs
                ) => MaruSymbol -> Eff xs SomeMaruPrimitive
lookupSymbol sym = do
  env <- getMaruEnv
  let cause = "A symbol '" <> unMaruSymbol sym <> "' is not found"
  includeFail cause . return $ M.lookup sym env
