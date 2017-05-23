{-# LANGUAGE OverloadedStrings #-}

-- | The parsers
module Maru.Parser
  ( module Maru.Parser.Type
  , parseTest
  , prettyPrint
  , P.parseErrorPretty
  , parse
  , debugParse
  ) where

import Control.Applicative ((<|>))
import Control.Monad (mapM_)
import Data.List (foldl')
import Data.Monoid ((<>))
import Maru.Parser.Type
import Maru.Type
import Text.Megaparsec (ParseError, Dec)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec as P


-- | Parse code to AST, and show AST and logs
parseTest :: SourceCode -> IO ()
parseTest = prettyPrint . debugParse

-- | Pretty print result of debugParse
prettyPrint :: (Either (ParseError MaruToken Dec) SExpr, [ParseLog]) -> IO ()
prettyPrint (parseResult, logs) = do
  let (messages, item) = foldl' sourt ([], "") logs
  mapM_ TIO.putStrLn $ reverse messages
  putStrLn ""
  TIO.putStrLn item
  putStrLn ""
  case parseResult of
    Left  e -> putStrLn $ P.parseErrorPretty e
    Right a -> putStrLn $ "Success:\n> " ++ show a
  where
    sourt (messages, itemResult) (Message msg)     = (msg:messages, itemResult)
    sourt (messages, itemResult) (ParsedItem item) = (messages, itemResult <> item)

-- | Parse code to AST without logs
parse :: SourceCode -> Either (ParseError MaruToken Dec) SExpr
parse = fst . debugParse

-- | Parse code to AST with logs
debugParse :: SourceCode -> (Either (ParseError MaruToken Dec) SExpr, [ParseLog])
debugParse = runMaruParser sexprParser


sexprParser :: MaruParser SExpr
sexprParser = do
  P.space
  atomParser <|> listParser
  where
    atomParser :: MaruParser SExpr
    atomParser = Atom <$> (numberParser <|> symbolParser)

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
