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

type ParseResult = Either ParseErrorResult SExpr


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


sexprParser :: MaruParser SExpr
sexprParser = do
  P.space
  atomParser <|> listParser
  where
    atomParser :: MaruParser SExpr
    atomParser = (P.try (numberParser) <|> P.try (boolParser) <|> symbolParser) <* P.space

    listParser :: MaruParser SExpr
    listParser = do
      P.char '('
      P.space
      xs <- P.many sexprParser
      P.space
      P.char ')'
      P.space
      return $ scottEncode xs

    numberParser :: MaruParser SExpr
    numberParser = P.try (naturalNumberParser) <|> P.try (positiveNumberParser) <|> negativeNumberParser

    boolParser :: MaruParser SExpr
    boolParser = return . AtomBool =<< judgeBool =<< P.string "true" <|> P.string "false"

    symbolParser :: MaruParser SExpr
    symbolParser = return . AtomSymbol . MTS.pack =<< P.some (P.noneOf ['\'', '(', ')', ' '])

    naturalNumberParser :: MaruParser SExpr
    naturalNumberParser = return . AtomInt =<< read' =<< P.some P.digitChar

    positiveNumberParser :: MaruParser SExpr
    positiveNumberParser = do
      P.char '+'
      numberParser

    negativeNumberParser :: MaruParser SExpr
    negativeNumberParser = do
      P.char '-'
      txt <- P.some P.digitChar
      AtomInt . negate <$> read' txt


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
