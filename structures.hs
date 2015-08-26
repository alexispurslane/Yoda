module Structures where

import Data.String.Utils
import Data.List

data YodaVal = Number Int
             | Id String
             | Func [YodaVal]
             | Str String
             | Decimal Float
             | Error String

instance Show YodaVal where
  show = showVal

unpackNumber :: YodaVal -> Int
unpackNumber x = case x of
  Number v  -> v
  otherwise -> 0

unpackString :: YodaVal -> String
unpackString x = case x of
  Str v     -> v
  Error v   -> v
  Number v  -> show v
  Decimal v -> show v
  Id v      -> v
  otherwise -> ""

unpackDecimal :: YodaVal -> Float
unpackDecimal x = case x of
  Decimal v -> v
  Number v  -> fromIntegral v
  otherwise -> 0.0

getBody :: YodaVal -> [YodaVal]
getBody (Func b) = b

showVal :: YodaVal -> String
showVal (Number v) = replace "-" "_" (show v)
showVal (Id v) = v
showVal (Func b) = "[ " ++ intercalate " " (map showVal b) ++ " ]"
showVal (Str v) = show v
showVal (Decimal v) = replace "-" "_" (show v)
showVal (Error e) = "ERROR: " ++ e
