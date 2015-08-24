module Parser where

import qualified Data.Text as T
import Data.Char

import Structures

allMatch f str = all (==True) (map f str)

strToVal :: String -> YodaVal
strToVal str
  | allMatch (not . isDigit) str = Id str
  | allMatch isDigit str = Number $ read str
  | otherwise = Error "Unknown form."

parse :: String -> [YodaVal]
parse str = map (strToVal . T.unpack) (T.splitOn (T.pack " ") (T.pack str))
