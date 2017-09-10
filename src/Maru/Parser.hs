{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The parsers
module Maru.Parser
  ( parseTest
  , prettyPrint
  , P.parseErrorPretty
  , parse
  , debugParse
  , ParseResult
  ) where

import Control.Applicative ((<|>))
import Control.Monad (mapM_)
import Maru.Type.Parser (ParseLog(..), ParseErrorResult, MaruParser, runMaruParser)
import Maru.Type.SExpr
import Maru.Type.SExpr (pattern AtomSymbol)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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


sexprParser :: MaruParser SExpr
sexprParser = do
  P.space
  atomParser <|> listParser
  where
    atomParser :: MaruParser SExpr
    atomParser = (numberParser <|> symbolParser) <* P.space

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
    numberParser = intParser

    symbolParser :: MaruParser SExpr
    symbolParser = AtomSymbol . MaruSymbol . T.pack <$> (P.some $ P.noneOf ['\'', '(', ')', ' '])

    intParser :: MaruParser SExpr
    intParser = AtomInt . read <$> P.some P.digitChar
