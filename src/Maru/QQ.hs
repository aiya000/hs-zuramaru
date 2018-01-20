{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Present compile time lisp (zuramaru) literals as quasi-quotes.
--
-- The compile time inline lisp is contained :sunny:
--
-- You can use 'Maru.QQ.ShortName' instead of these functions,
-- if you don't worry name conflictions.
--
-- These requires to
-- `
-- import Maru.Type.SExpr (SExpr(..), MaruSymbol(..))
-- `
-- and add `text` to your package.yaml or {project}.cabal (below is a package.yaml example)
-- `
-- dependencies:
--    - text
-- `
--
-- and optionally requirements are existent, please see below.
--
-- If you want to use as patterns
-- (e.g. `let ![parse|123|] = AtomInt 123`)
-- `
-- {-# LANGUAGE OverloadedStrings #-}
-- `
--
-- If you want to use as types
-- (e.g. `f :: [parse|(1 2)|]`)
-- `
-- {-# LANGUAGE DataKinds #-}
-- `
module Maru.QQ
  ( parse
  , parsePreprocess
  , zurae
  ) where

import Control.Arrow ((>>>))
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Maru.Type (SExpr(..), MaruSymbol(..), CallowSExpr(..))
import qualified Data.Text as T
import qualified Maru.Eval as Maru
import qualified Maru.Parser as Maru
import qualified Maru.Preprocessor as Maru

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedStrings
-- >>> import Data.Singletons (sing, fromSing)
-- >>> import Maru.Type.Parser (ParseErrorResult)
-- >>> import Maru.Type.SExpr (SourceCode)
-- >>> import Maru.Type.TypeLevel (HighSExpr(..), Sing(..))
-- >>> import System.IO.Silently (silence)

--TODO: Implement tests to check the compiler errors (See 'Maru.QQ.parse', 'Maru.QQ.zurae' documentation for conditions of the compile error)
--      (What can I implement it ?)


textE :: Text -> Exp
textE t =
  let x = T.unpack t
  in VarE (mkName "Data.Text.pack") `AppE` LitE (StringL x)

intL :: Int -> Lit
intL = IntegerL . fromIntegral

boolP :: Bool -> Pat
boolP x = ConP (mkName $ show x) []

-- | A value `x :: Int` as a type `x :: 'Nat'`
intT :: Int -> Type
intT = LitT . NumTyLit . fromIntegral

-- | A value `x :: Bool` as a type `x :: 'Bool`
boolT :: Bool -> Type
boolT = PromotedT . mkName . show

-- | A value 'x :: Text' as a type `x :: 'Symbol'`
textSym :: Text -> Type
textSym = LitT . StrTyLit . T.unpack


toExp :: SExpr -> Exp
toExp (Cons x y)   = ConE (mkName "Cons") `AppE` ParensE (toExp x) `AppE` ParensE (toExp y)
toExp (Quote x)    = ConE (mkName "Quote") `AppE` ParensE (toExp x)
toExp Nil          = ConE (mkName "Nil")
toExp (AtomInt x)  = ConE (mkName "AtomInt") `AppE` LitE (intL x)
toExp (AtomBool x) = ConE (mkName "AtomBool") `AppE` ConE (mkName $ show x)
toExp (AtomSymbol (MaruSymbol x)) = ConE (mkName "AtomSymbol") `AppE`
                                      ParensE (ConE (mkName "MaruSymbol") `AppE` textE x)


toPat :: SExpr -> Pat
toPat (Cons x y)   = ConP (mkName "Cons") [toPat x, toPat y]
toPat (Quote x)    = ConP (mkName "Quote") [toPat x]
toPat Nil          = ConP (mkName "Nil") []
toPat (AtomInt x)  = ConP (mkName "AtomInt") [LitP $ intL x]
toPat (AtomBool x) = ConP (mkName "AtomBool") [boolP x]
-- This requires OverloadedStrings
toPat (AtomSymbol (MaruSymbol x)) = ConP (mkName "AtomSymbol") [ConP (mkName "MaruSymbol") [LitP . StringL $ T.unpack x]]


toType :: SExpr -> Type
toType (Cons x y)   = PromotedT (mkName "HCons") `AppT` ParensT (toType x) `AppT` ParensT (toType y)
toType (Quote x)    = PromotedT (mkName "HQuote") `AppT` ParensT (toType x)
toType Nil          = PromotedT (mkName "HNil")
--NOTE: Should specify a branch when `x < 0` ? or NumTyLit satisfies ?
toType (AtomInt x)  = PromotedT (mkName "HAtomInt") `AppT` intT x
toType (AtomBool x) = PromotedT (mkName "HAtomBool") `AppT` boolT x
toType (AtomSymbol (MaruSymbol x)) = PromotedT (mkName "HAtomSymbol") `AppT` textSym x


-- |
-- Expand a code to 'SExpr'
-- if it can be parsed by 'Maru.Parser.parse'
-- (or to be compiled error).
--
-- Occure the compile error if it cannot be parsed.
--
-- >>> :{
-- >>> let p :: SourceCode -> Either ParseErrorResult SExpr
-- >>>     p x = growUp <$> Maru.parse x -- with the empty preprocessor (growUp)
-- >>> :}
--
-- As expressions
--
-- >>> Right [parse|123|] == p "123"
-- True
-- >>> Right [parse|abc|] == p "abc"
-- True
-- >>> Right [parse|(123 456)|] == p "(123 456)"
-- True
--
-- As patterns
--
-- >>> case AtomInt 123 of; [parse|123|] -> "good"
-- "good"
-- >>> case AtomInt 000 of; [parse|123|] -> "bad"; AtomInt _ -> "good"
-- "good"
-- >>> :set -XOverloadedStrings
-- >>> case AtomSymbol "x" of; [parse|x|] -> "good"
-- "good"
-- >>> case Quote (AtomInt 10) of; [parse|'10|] -> "good"
-- "good"
-- >>> case Cons (AtomSymbol "x") (Cons (AtomInt 10) Nil) of; [parse|(x 10)|] -> "good"
-- "good"
--
-- As types (compile time calculations)
--
-- >>> fromSing (sing :: Sing [parse|10|])
-- AtomInt 10
-- >>> fromSing (sing :: Sing [parse|konoko|])
-- AtomSymbol "konoko"
-- >>> fromSing (sing :: Sing [parse|(1 2 3)|])
-- Cons (AtomInt 1) (Cons (AtomInt 2) (Cons (AtomInt 3) Nil))
parse :: QuasiQuoter
parse = QuasiQuoter
  { quoteExp  = power toExp
  , quotePat  = power toPat
  , quoteType = power toType
  , quoteDec  = error "Maru.QQ.parse: the expansion to `[Dec]` (`quoteDec`) isn't support supported"
  }
  where
    power :: (SExpr -> a) -> String -> Q a
    power f = T.pack >>> Maru.parse >>> \case
      Left  e -> fail $ Maru.parseErrorPretty e
      Right a -> return . f $ growUp a


-- |
-- Similar to 'parse' but 'Maru.preprocessor.preprocess' is appended
--
-- >>> :{
-- >>> let pp :: SourceCode -> Either ParseErrorResult SExpr
-- >>>     pp code = Maru.preprocess <$> Maru.parse code
-- >>> :}
--
-- >>> Right [parsePreprocess|sugar|] == pp "sugar"
-- True
-- >>> Right [parsePreprocess|10|] == pp "10"
-- True
-- >>> Right [parsePreprocess|(1 2 3)|] == pp "(1 2 3)"
-- True
-- >>> Right [parsePreprocess|'10|] == pp "'10"
-- True
parsePreprocess :: QuasiQuoter
parsePreprocess = QuasiQuoter
  { quoteExp  = power toExp
  , quotePat  = power toPat
  , quoteType = power toType
  , quoteDec  = error "Maru.QQ.parsePreprocess: the expansion to `[Dec]` (`quoteDec`) isn't support supported"
  }
  where
    power :: (SExpr -> a) -> String -> Q a
    power f = T.pack >>> Maru.parse >>> \case
      Left  e -> fail $ Maru.parseErrorPretty e
      Right a -> return . f $ Maru.preprocess a


-- |
-- Similar to 'parsePreprocess' but it is ran in the compile time,
-- Occure the side effects,
-- and Return the evaluated result.
--
-- Occure the compile error if the code throws exceptions.
--
-- この関数名zuraeの発音はずら〜（ずらあ）です。
-- (the e suffix means an 'e'valuation)
--
-- NOTICE:
-- Please be careful to the halting problem.
-- The halting safety is abandon
-- because this ('Maru.Eval.eval') is turing-complete in the compile time :D
--
-- >>> :{
-- >>> let force :: SourceCode -> IO SExpr
-- >>>     force code = do
-- >>>        let (Right sexpr) = Maru.preprocess <$> Maru.parse code
-- >>>        Right (result, _, _) <- silence $ Maru.eval Maru.initialEnv sexpr
-- >>>        return result
-- >>> :}
--
-- >>> (==) <$> return [zurae|10|] <*> force "10"
-- True
-- >>> (==) <$> return [zurae|'sugar|] <*> force "'sugar"
-- True
-- >>> (==) <$> return [zurae|'(1 2 3)|] <*> force "'(1 2 3)"
-- True
-- >>> (==) <$> return [zurae|(print 10)|] <*> force "(print 10)"
-- 10True
zurae :: QuasiQuoter
zurae = QuasiQuoter
  { quoteExp  = power toExp
  , quotePat  = power toPat
  , quoteType = power toType
  , quoteDec  = error "Maru.QQ.zurae: the expansion to `[Dec]` (`quoteDec`) isn't support supported"
  }
  where
    power :: (SExpr -> a) -> String -> Q a
    power f = T.pack >>> Maru.parse >>> fmap Maru.preprocess >>> \case
      Left e -> fail $ Maru.parseErrorPretty e
      Right sexpr -> do
        result <- runIO $ Maru.eval Maru.initialEnv sexpr
        case result of
          Left  e -> fail $ "Maru.QQ.zurae: an error is occured in the compile time: " ++ show e
          Right (sexpr, _, _) -> return $ f sexpr
