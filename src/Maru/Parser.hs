{-# LANGUAGE OverloadedStrings #-}

-- | The parsers
module Maru.Parser
  ( parseTest
  , prettyPrint
  , P.parseErrorPretty
  , prettyShowLogs
  , prettyPrintLogs
  , parse
  , debugParse
  ) where

import Control.Applicative ((<|>))
import Control.Monad (mapM_)
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Text (Text)
import Maru.Type.Parser (ParseLog(..), ParseErrorResult, MaruParser, runMaruParser)
import Maru.Type.SExpr
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
  prettyPrintLogs logs
  putStrLn ""
  case parseResult of
    Left  e -> putStrLn $ P.parseErrorPretty e
    Right a -> putStrLn $ "Success:\n> " ++ show a

-- |
-- Convert @[ParseLog]@ to human readable logs.
-- 
-- @Message@ and @ParsedItem@ are devided.
-- The first element is a result of @Message@.
-- The second element is a result of @ParsedItem@.
prettyShowLogs :: [ParseLog] -> ([Text], Text)
prettyShowLogs logs =
  let (messages, item) = foldl' sourt ([], "") logs
  in (reverse messages, item)
  where
    sourt (messages, itemResult) (Message msg)     = (msg:messages, itemResult)
    sourt (messages, itemResult) (ParsedItem item) = (messages, itemResult <> item)

-- | Pretty print @[ParseLog]@
prettyPrintLogs :: [ParseLog] -> IO ()
prettyPrintLogs logs = do
  let (message, item) = prettyShowLogs logs
  mapM_ TIO.putStrLn $ message
  putStrLn ""
  TIO.putStrLn item

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
    atomParser = Atom <$> (numberParser <|> symbolParser) <* P.space

    listParser :: MaruParser SExpr
    listParser = do
      P.char '('
      P.space
      xs <- P.many sexprParser
      P.space
      P.char ')'
      P.space
      return $ scottEncode xs

    numberParser :: MaruParser MaruTerm
    numberParser = intParser

    symbolParser :: MaruParser MaruTerm
    symbolParser = TermSymbol . T.pack <$> (P.some $ P.noneOf ['\'', '(', ')', ' '])

    intParser :: MaruParser MaruTerm
    intParser = TermInt . read <$> P.some P.digitChar
