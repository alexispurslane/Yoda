module Main where

import qualified Data.Text as T
import Data.Char

import qualified Data.Map as Map

data YodaVal = Number Int
             | Str String
             | Id String
             | Decimal Float
             | Error String
             deriving (Show)

defaultEnv :: Map.Map String (YodaVal -> YodaVal -> YodaVal, Int)
defaultEnv = Map.fromList [("+", (numericBinop (+), 2)),
                           ("-", (numericBinop (-), 2)),
                           ("*", (numericBinop (*), 2)),
                           ("/", (numericBinop div, 2))]

numericBinop :: (Int -> Int -> Int) -> YodaVal -> YodaVal -> YodaVal
numericBinop op x y = Number (unpackNumber x `op` unpackNumber y)

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

push :: a -> [a] -> [a]
push x xs = xs ++ [x]

pop :: [a] -> (a, [a])
pop xs = (last xs, init xs)

eval env e s = case e of
  v@(Number _)  -> v : s
  v@(Str _)     -> v : s
  v@(Decimal _) -> v : s
  (Id v)        -> exec s v (env Map.! v)
  otherwise     -> [Error "Unknown form or expression."]
  where
    exec s i f = case f of
      (fn, n)   -> [Str $ "Not implemented. Function " ++ i ++ " of " ++ show n ++ " arguments."]


run :: [YodaVal] -> [YodaVal] -> Map.Map String (YodaVal -> YodaVal -> YodaVal, Int) -> [YodaVal]
run [] s _ = s
run exps stack env = let r = eval env (head exps) stack
                     in case r of
                       v@[Error _] -> v
                       otherwise -> run (tail exps) r env
