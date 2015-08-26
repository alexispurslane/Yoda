module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)

import Data.Char
import Data.String.Utils
import Data.Either
import Data.List
import qualified Data.Text as T

import Control.Monad

import Structures

import Debug.Trace

symbol :: Parser Char
symbol = oneOf "!#$%&|*+/:<=>?@^_~{}()\\'-"

parseId :: Parser YodaVal
parseId = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ Id (first:rest)

parseNumber :: Parser YodaVal
parseNumber = do
  x <- try (many1 (digit <|> dot <|> sign)) <|> (many1 (digit <|> dot))
  return . readWrap $ x
  where readWrap sn = if '.' `elem` sn
                         then Decimal (read (replace "_" "-" sn))
                         else Number (read (replace "_" "-" sn))
        sign = char '_'
        dot = char '.'


spaces :: Parser ()
spaces = skipMany1 (oneOf "\n\r\f\t ")

parseString :: Parser YodaVal
parseString = do
  char '"'
  x <- many (noneOf "\\\"")
  char '"'
  return $ Str x

parseList :: Parser YodaVal
parseList = liftM Func $ spaces >> endBy parseExpr spaces

parseExpr :: Parser YodaVal
parseExpr = parseNumber
            <|> parseId
            <|> parseString
            <|> do
              char '['
              x <- try parseList
              char ']'

              return x

parseExprs :: Parser [YodaVal]
parseExprs = sepBy parseExpr spaces

parseAll str = head $ rights [parse parseExprs "Yoda" str]
