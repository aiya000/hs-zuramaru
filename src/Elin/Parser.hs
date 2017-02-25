{-# LANGUAGE OverloadedStrings #-}

module Elin.Parser
  ( ElinParser (..)
  , parseTest
  , parse
  , debugParse
  ) where

import Control.Applicative ((<|>))
import Control.Monad (mapM_)
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Text (Text)
import Elin.Parser.Type (ParseLog(..), ElinParser(..), runElinParser, tellMsg, tellItem, increaseNestLevel, decreaseNestLevel)
import Elin.Type
import Text.Megaparsec (ParseError, Dec)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec as P


-- | Parse code, and show parse logs and result
parseTest :: SourceCode -> IO ()
parseTest x = do
  let (parseResult, logs) = debugParse x
      (messages, item)    = foldl' sourt ([], "") logs
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

-- | Parse code
parse :: SourceCode -> Either (ParseError Token Dec) SExpr
parse = fst . debugParse

-- | Parse code with logging
debugParse :: SourceCode -> (Either (ParseError Token Dec) SExpr, [ParseLog])
debugParse = runElinParser sexprParser


-- | Parse a text and tell it if the parsing is succeed
identifier' :: ElinParser Text
identifier' = do
  ident <- identifier
  tellItem $ ident <> " "
  return ident
  where
    -- Parse an identifier
    identifier :: ElinParser Text
    identifier = T.pack <$> (P.some $ P.noneOf ['\'', '(', ')', ' '])

-- | Parse a char and tell it if the parsing is succeed
char' :: Char -> ElinParser Char
char' c = do
  x <- P.char c
  tellItem $ T.singleton c
  return x

--NOTE: Can I logging with more elegant code?
sexprParser :: ElinParser SExpr
sexprParser = do
  P.space
  tellMsg "-> sexprParser"
  result <- consParser <|> quoteParser <|> symbolParser
  tellMsg "<- leave from sexprParser"
  return result
  where
    consParser = do
      char' '('
      tellMsg ".  consParser"
      increaseNestLevel
      listParser
    quoteParser = do
      char' '\''
      tellMsg ".  quoteParser"
      increaseNestLevel
      x <- sexprParser
      return $ Cons (Symbol "quote") x
    symbolParser = do
      ident <- identifier'
      tellMsg ".  symbolParser"
      decreaseNestLevel
      return $ Symbol ident

listParser :: ElinParser SExpr
listParser = do
  P.space
  tellMsg "-> listParser"
  result <- endOfParen <|> childSExprParser
  tellMsg "<- leave from listParser"
  decreaseNestLevel
  return result
  where
    endOfParen = do
      char' ')'
      tellMsg ".  endOfParen"
      return Nil
    childSExprParser = do
      tellMsg ".  childSExprParser"
      increaseNestLevel
      x <- sexprParser
      increaseNestLevel
      y <- listParser
      return $ Cons x y
