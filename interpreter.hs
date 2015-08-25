module Interpreter where

import Structures
import Parser

import qualified Data.Map as Map
import Data.Either

defaultEnv :: Map.Map String ([YodaVal] -> YodaVal, Int)
defaultEnv = Map.fromList [("+", (numericBinop (+), 2)),
                           ("-", (numericBinop (-), 2)),
                           ("*", (numericBinop (*), 2)),
                           ("/", (numericBinop div, 2)),
                           ("^", (numericBinop (^), 2)),
                           ("%", (numericBinop mod, 2))]

numericBinop :: (Int -> Int -> Int) -> [YodaVal] -> YodaVal
numericBinop op args = Number (unpackNumber (head args) `op` unpackNumber (head $ tail args))

execute :: [YodaVal] -> String -> ([YodaVal] -> YodaVal, Int) -> [YodaVal]
execute s i f = case f of
  (fn, n)   -> if length s >= n
                  then (reverse . drop n $ reverse s) ++ [(fn . reverse) . take n $ reverse s]
                  else [Error "Data stack underflow."]

evalIdent :: Map.Map String ([YodaVal] -> YodaVal, Int) -> YodaVal -> [YodaVal] -> [YodaVal]
evalIdent env e s = case e of
  v@(Number _)  -> s ++ [v]
  v@(Str _)     -> s ++ [v]
  v@(Decimal _) -> s ++ [v]
  v@(Func _)    -> s ++ [v]
  Id "clear"    -> []
  Id "call"     -> run (getBody (last s)) (init s) env
  Id v          -> case Map.lookup v env of
                     Just res -> execute s v res
                     Nothing  -> [Error "Undefined function or name."]
  otherwise     -> [Error "Unknown form or expression."]


run :: [YodaVal] -> [YodaVal] -> Map.Map String ([YodaVal] -> YodaVal, Int) -> [YodaVal]
run [] s _ = s
run exps stack env = if length exps > 0
                     then let r = evalIdent env (head exps) stack
                          in case r of
                          v@[Error _] -> v
                          otherwise -> run (tail exps) r env
                       else []

eval str = run (parseAll str)
