module Interpreter where

import qualified Data.Text as T
import Data.Char

import qualified Data.Map as Map

data YodaVal = Number Int
             | Str String
             | Id String
             | Decimal Float
             | Error String
             deriving (Show)

defaultEnv :: Map.Map String ([YodaVal] -> YodaVal, Int)
defaultEnv = Map.fromList [("+", (numericBinop (+), 2)),
                           ("-", (numericBinop (-), 2)),
                           ("*", (numericBinop (*), 2)),
                           ("/", (numericBinop div, 2)),
                           ("^", (numericBinop (^), 2)),
                           ("%", (numericBinop mod, 2))]

numericBinop :: (Int -> Int -> Int) -> [YodaVal] -> YodaVal
numericBinop op args = Number (unpackNumber (head args) `op` unpackNumber (head $ tail args))

unpackNumber :: YodaVal -> Int
unpackNumber x = case x of
  Number v -> v
  otherwise -> 0

unpackString :: YodaVal -> String
unpackString x = case x of
  Str v -> v
  otherwise -> ""

unpackDecimal :: YodaVal -> Float
unpackDecimal x = case x of
  Decimal v -> v
  Number v -> fromIntegral v
  otherwise -> 0.0

allMatch f str = all (==True) (map f str)

strToVal :: String -> YodaVal
strToVal str
  | allMatch (not . isDigit) str = Id str
  | allMatch isDigit str = Number $ read str
  | otherwise = Error "Unknown form."

parse :: String -> [YodaVal]
parse str = map (strToVal . T.unpack) (T.splitOn (T.pack " ") (T.pack str))

execute :: [YodaVal] -> String -> ([YodaVal] -> YodaVal, Int) -> [YodaVal]
execute s i f = case f of
  (fn, n)   -> if length s >= n
                  then reverse (drop n $ reverse s) ++ [fn [last (take n $ reverse s),
                                                            second (reverse $ take n $ reverse s)]]
                  else [Error "Data stack underflow."]
  where second = head . tail

evalIdent :: Map.Map String ([YodaVal] -> YodaVal, Int) -> YodaVal -> [YodaVal] -> [YodaVal]
evalIdent env e s = case e of
  v@(Number _)  -> s ++ [v]
  v@(Str _)     -> s ++ [v]
  v@(Decimal _) -> s ++ [v]
  Id "clear"    -> []
  Id v          -> if Map.member v env
                      then execute s v (env Map.! v)
                      else [Error "Undefined function or name."]
  otherwise     -> [Error "Unknown form or expression."]


run :: [YodaVal] -> [YodaVal] -> Map.Map String ([YodaVal] -> YodaVal, Int) -> [YodaVal]
run [] s _ = s
run exps stack env = let r = evalIdent env (head exps) stack
                     in case r of
                       v@[Error _] -> v
                       otherwise -> run (tail exps) r env

eval str = run (parse str)
