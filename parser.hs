module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)

import Data.Char
import Data.Either
import Data.List
import qualified Data.Text as T

import Control.Monad

import Structures

import Debug.Trace

symbol :: Parser Char
symbol = oneOf "!#$%&|*+/:<=>?@^_~{}()\\'"

sign = char '-'
dot = char '.'

parseId :: Parser YodaVal
parseId = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol <|> sign)
  return $ Id (first:rest)

parseNumber :: Parser YodaVal
parseNumber = liftM readWrap $ many1 (digit <|> sign <|> dot)
  where readWrap sn = if '.' `elem` sn
                         then Decimal (read sn)
                         else Number (read sn)


spaces :: Parser ()
spaces = skipMany1 (oneOf "\n\r\f\t ")

parseString :: Parser YodaVal
parseString = do
  char '"'
  x <- many (noneOf "\\\"")
  char '"'
  return $ Str x

parseList :: Parser YodaVal
parseList = liftM Func $ spaces >> (endBy (parseExpr) spaces)

parseExpr :: Parser YodaVal
parseExpr = parseId
            <|> parseNumber
            <|> parseString
            <|> do
              char '['
              x <- try parseList
              trace (show x) (char ']')

              return x

parseExprs :: Parser [YodaVal]
parseExprs = sepBy parseExpr spaces

parseAll str = head $ rights [parse parseExprs "Yoda" str]
