module Structures where

data YodaVal = Number Int
             | Str String
             | Id String
             | Func [YodaVal]
             | Decimal Float
             | Error String
             deriving (Show)

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
