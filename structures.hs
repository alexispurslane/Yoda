{-|
Module      : Structures
Description : The data type for Yoda values and parsed code, along with some utility functions.
Copyright   : (c) Christopher Dumas, 2015
License     : GPL-3
Maintainer  : christopherdumas@gmail.com
Stability   : experimental
Portability : POSIX

The data type represented here shows the possible syntactic forms of Yoda, represented in Haskell for exectution and storage.
-}
module Structures where

import Data.String.Utils
import Data.List

-- | Represents the possible types of data in Yoda
data YodaVal = Number Int
               -- ^ Represents a number in Yoda.
             | Id String
               -- ^ Represents an identifier in Yoda.
             | Func [YodaVal]
               -- ^ A function in Yoda is a list of YodaVals, in this case the parsed code
               -- inside the function. This code can be directly fed into the interpreter to be run.
             | Str String
               -- ^ A Yoda String.
             | Decimal Float
               -- ^ This represents a decimal in Yoda.
             | Error String
               -- ^ An error type, for error correction in Yoda.

-- | Implements a show function for YodaVal
instance Show YodaVal where
  show = showVal

-- | /unpackNumber x/ returns the unwrapped Number x, or 0.
unpackNumber :: YodaVal -> Int
unpackNumber x = case x of
  Number v  -> v
  otherwise -> 0

-- | /unpackString x/ changes x into a string if it is not, using show. Otherwise if it is a Str, Error, or Id, returns the value.
unpackString :: YodaVal -> String
unpackString x = case x of
  Str v     -> v
  Error v   -> v
  Number v  -> show v
  Decimal v -> show v
  Id v      -> v
  otherwise -> ""

-- | /unpackDecimal x/ makes a Number into a Float, or just returns the unwrapped value of a Decimal. If x is neither a Decimal or a Number, it returns 0.0.
unpackDecimal :: YodaVal -> Float
unpackDecimal x = case x of
  Decimal v -> v
  Number v  -> fromIntegral v
  otherwise -> 0.0

-- | Returns a function body, otherwise throws an error
getBody :: YodaVal -> [YodaVal]
getBody (Func b) = b

-- | Formats a Yoda value.
showVal :: YodaVal -> String
showVal (Number v) = replace "-" "_" (show v)
showVal (Id v) = v
showVal (Func b) = "[ " ++ intercalate " " (map showVal b) ++ " ]"
showVal (Str v) = show v
showVal (Decimal v) = replace "-" "_" (show v)
showVal (Error e) = "ERROR: " ++ e
