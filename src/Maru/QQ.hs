-- |
-- Present compile time lisp (zuramaru) literals as quasi-quotes.
--
-- The compile time inline lisp is contained :sunny:
--
-- You can use 'Maru.QQ.ShortName' instead of these functions,
-- if you don't worry name conflictions.
module Maru.QQ
  ( parse
  , parsePreprocess
  , zurae
  ) where

import Language.Haskell.TH.Quote (QuasiQuoter)

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Maru.Type.Parser (ParseErrorResult)
-- >>> import Maru.Type.SExpr (SourceCode, SExpr)
-- >>> import qualified Maru.Parser as Maru
-- >>> import qualified Maru.Preprocessor as Maru

--TODO: Implement tests to check the compiler errors (See 'Maru.QQ.parse', 'Maru.QQ.zurae' documentation for conditions of the compile error)
--      (What can I implement it ?)


-- |
-- Expand a code to 'SExpr' if it can be parsed by 'Maru.Parser.parse'.
--
-- Occure the compile error if it cannot be parsed.
--
-- >>> [parse|123|] == Maru.parse "123"
-- >>> [parse|abc|] == Maru.parse "abc"
-- >>> [parse|(123 456)|] == Maru.parse "(123 456)"
parse :: QuasiQuoter
parse = undefined


-- |
-- Similar to 'parse' but 'Maru.preprocessor.preprocess' is appended
--
-- >>> :{
-- >>> let pp :: SourceCode -> Either ParseErrorResult SExpr
-- >>>     pp code = Maru.preprocess <$> Maru.parse code
-- >>> :}
--
-- >>> [parsePreprocess|sugar|] == Maru.parse "sugar"
parsePreprocess :: QuasiQuoter
parsePreprocess = undefined


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
-- >>> [zurae|(print 10)|]
-- 10
-- Nil
-- >>> [zurae|10|]
-- AtomInt 10
-- >>> [zurae|'(1 2 3)|]
-- Cons (Cons (AtomInt 1) (Cons (AtomInt 2) (Cons (AtomInt 3) Nil)))
zurae :: QuasiQuoter
zurae = undefined
