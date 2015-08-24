module Parser where

import qualified Data.Text as T
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Char
import Control.Monad
import Data.Either

import Structures

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~{}()[]\\'"

parseId :: Parser YodaVal
parseId = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ Id (first:rest)

parseNumber = do
  liftM readWrap $ many1 digit
  where readWrap = Number . read

spaces :: Parser ()
spaces = skipMany1 (oneOf "\n\r\f\t ")

parseString :: Parser YodaVal
parseString = do
  char '"'
  x <- many (noneOf "\\\"")
  char '"'
  return $ Str x

parseExpr :: Parser YodaVal
parseExpr = parseId
            <|> parseNumber
            <|> parseString

parseExprs :: Parser [YodaVal]
parseExprs = sepBy parseExpr spaces

parseAll str = (head $ rights [parse parseExprs "Yoda" str])
