{-# LANGUAGE OverloadedStrings #-}

-- | The parsers
module Maru.Parser
  ( parseTest
  , prettyPrint
  , parseErrorPretty
  , parse
  , debugParse
  , ParseResult
  ) where

import Control.Applicative ((<|>))
import Control.Monad (mapM_)
import Control.Monad.Fail (fail)
import Maru.Type.Parser (ParseLog(..), ParseErrorResult, MaruParser, runMaruParser)
import Maru.Type.SExpr
import Prelude hiding (fail)
import Safe (readMay)
import qualified Data.Text.IO as TIO
import qualified Maru.Type.SExpr as MTS
import qualified Text.Megaparsec as P

type ParseResult = Either ParseErrorResult CallowSExpr


-- | Parse code to AST, and show AST and logs
parseTest :: SourceCode -> IO ()
parseTest = prettyPrint . debugParse

-- | Pretty print result of debugParse
prettyPrint :: (ParseResult, [ParseLog]) -> IO ()
prettyPrint (parseResult, logs) = do
  mapM_ (TIO.putStrLn . unParseLog) logs
  putStrLn ""
  case parseResult of
    Left  e -> putStrLn $ P.parseErrorPretty e
    Right a -> putStrLn $ "Success:\n> " ++ show a

-- | Parse code to AST without logs
parse :: SourceCode -> ParseResult
parse = fst . debugParse

-- | Parse code to AST with logs
debugParse :: SourceCode -> (ParseResult, [ParseLog])
debugParse = runMaruParser sexprParser

parseErrorPretty :: ParseErrorResult -> String
parseErrorPretty = P.parseErrorPretty


sexprParser :: MaruParser CallowSExpr
sexprParser = do
  P.space
  quoteParser <<> atomParser <<> listParser
  where
    -- but "(quote x)" is not parsed to `Quote (AtomSymbol "x")` in here,
    -- it is parsed to `Cons (AtomSymbol "quote") (Cons (AtomSymbol "x") Nil)`
    -- in the outer of here
    quoteParser :: MaruParser CallowSExpr
    quoteParser = do
      P.char '\''
      Quote' <$> sexprParser

    atomParser :: MaruParser CallowSExpr
    atomParser = (numberParser <<> boolParser <<> symbolParser) <* P.space

    listParser :: MaruParser CallowSExpr
    listParser = do
      P.char '('
      P.space
      xs <- P.many sexprParser
      P.space
      P.char ')'
      P.space
      return $ scottEncode' xs

    numberParser :: MaruParser CallowSExpr
    numberParser = naturalNumberParser <<> positiveNumberParser <<> negativeNumberParser

    boolParser :: MaruParser CallowSExpr
    boolParser = return . AtomBool' =<< judgeBool =<< P.string "true" <|> P.string "false"

    symbolParser :: MaruParser CallowSExpr
    symbolParser = return . AtomSymbol' . MTS.pack =<< P.some (P.noneOf ['\'', '(', ')', ' '])

    naturalNumberParser :: MaruParser CallowSExpr
    naturalNumberParser = return . AtomInt' =<< read' =<< P.some P.digitChar

    positiveNumberParser :: MaruParser CallowSExpr
    positiveNumberParser = do
      P.char '+'
      numberParser

    negativeNumberParser :: MaruParser CallowSExpr
    negativeNumberParser = do
      P.char '-'
      txt <- P.some P.digitChar
      AtomInt' . negate <$> read' txt


-- |
-- Return the constant successive parser for `Bool` if the string is "true" or "false".
-- Return the constant failure parser if it is the another string.
judgeBool :: String -> MaruParser Bool
judgeBool "true"  = return True
judgeBool "false" = return False
judgeBool _       = fail "boolParser is failed"

-- |
-- Read it,
-- but if the reading is failed, the whole of `MaruParser` context to be failure.
-- Otherwise, the result into the context and Return it.
read' :: Read a => String -> MaruParser a
read' x =
  case readMay x of
    Nothing -> fail "read': fatal error! a reading the `String` to the `Read` value is failed"
    Just x' -> return x'


infixr 5 <<>
-- | Don't consume the tokens if the left parser is failed
(<<>) :: MaruParser a -> MaruParser a -> MaruParser a
x <<> y = P.try x <|> y
