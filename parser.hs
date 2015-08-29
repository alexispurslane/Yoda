{-|
Module      : Parser
Description : A Parsec parser that controls how Yoda is parsed and turned into an AST.
Copyright   : (c) Christopher Dumas, 2015
License     : GPL-3
Maintainer  : christopherdumas@gmail.com
Stability   : experimental
Portability : POSIX

This module contains all of the necissary logic for parsing Yoda, turning it into an AST, and attaching some metadata.
I use Parsec to parse Yoda.
-}
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

-- | Matches one of the symbols that are allowed in Yoda identifiers.
symbol :: Parser Char
symbol = oneOf "!#$%&|*+/:<=>?@^_~{}()\\'-"

-- | Matches a Yoda identifier: The first character is a letter or a symbol,
-- the rest of the characters are either a letter, a digit, or a symbol. If the identifier is "t" then returns Boolean True, if it is "f", returns Boolean False.
parseId :: Parser YodaVal
parseId = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)

  case Id $ first:rest of
    Id "t" -> return $ Boolean True
    Id "f" -> return $ Boolean False
    v@_    -> return $ v

-- | Parsers a integer or a floating point number. Minor oddity: A negitive sign is an underscore in Yoda:
-- _1 is a negative number.
parseNumber :: Parser YodaVal
parseNumber = do
  x <- try (many1 (digit <|> dot <|> sign)) <|> (many1 (digit <|> dot))
  return . readWrap $ x
  where readWrap sn = if '.' `elem` sn
                         then Decimal (read (replace "_" "-" sn))
                         else Number (read (replace "_" "-" sn))
        sign = char '_'
        dot = char '.'

-- | Matches and ignores whitespace.
spaces :: Parser ()
spaces = skipMany1 (oneOf "\n\r\f\t ")

-- | Parses a Yoda string. Only double quotes work.
parseString :: Parser YodaVal
parseString = do
  char '"'
  x <- many (noneOf "\\\"")
  char '"'
  return $ Str x

-- | Parses a bunch of space separated expressions, starting and ending with a space.
parseList :: Parser YodaVal
parseList = liftM Func $ spaces >> endBy parseExpr spaces

-- | Parses either a number, an identifier, or a block.
parseExpr :: Parser YodaVal
parseExpr = parseNumber
            <|> parseId
            <|> parseString
            <|> do
              char '['
              x <- try parseList
              char ']'

              return x

-- | Parses multiple expressions.
parseExprs :: Parser [YodaVal]
parseExprs = sepBy parseExpr spaces

-- | Parses a string, using the PArsec parse function.
parseAll str = head $ rights [parse parseExprs "Yoda" str]
