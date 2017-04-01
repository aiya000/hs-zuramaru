{-# LANGUAGE OverloadedStrings #-}

-- | The parsers
module Elin.Parser
  ( module Elin.Parser.Type
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
import Elin.Parser.Type
import Elin.Type
import Text.Megaparsec (ParseError, Dec)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec as P


-- | Parse code to AST, and show AST and logs
parseTest :: SourceCode -> IO ()
parseTest = prettyPrint . debugParse

-- | Pretty print result of debugParse
prettyPrint :: (Either (ParseError Token Dec) SExpr, [ParseLog]) -> IO ()
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
parse :: SourceCode -> Either (ParseError Token Dec) SExpr
parse = fst . debugParse

-- | Parse code to AST with logs
debugParse :: SourceCode -> (Either (ParseError Token Dec) SExpr, [ParseLog])
debugParse = runElinParser sexprParser


-- | Parse a char and `tell` its result as an item, if the parsing is succeed
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
  result <- consParser <|> quoteParser <|> termParser
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
      return $ Quote x
    termParser = do
      term <- intParser <|> nameParser
      tellMsg ".  termParser"
      decreaseNestLevel
      return $ TermItem term

    intParser :: ElinParser ElinTerm
    intParser = do
      digit <- P.some P.digitChar
      let digitText = T.pack . show $ digit
      tellItem $ digitText <> " "
      return $ TermInt $ read digit

    nameParser :: ElinParser ElinTerm
    nameParser = do
      name <- T.pack <$> (P.some $ P.noneOf ['\'', '(', ')', ' '])
      tellItem $ name <> " "
      return $ TermName name

-- Terminate or continue the parsing
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
