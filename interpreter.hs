module Interpreter where

import Structures
import Parser

import qualified Data.Map as Map
import Data.Either
import Debug.Trace
type Env = Map.Map String ([YodaVal] -> YodaVal, Int)

defaultEnv :: Env
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

languageFunc :: YodaVal -> Env -> [YodaVal] -> YodaVal
languageFunc f e a = last (fst (run (getBody f) a e))

evalIdent :: Env -> YodaVal -> [YodaVal] -> ([YodaVal], Env)
evalIdent env e s = case e of
  v@(Number _)  -> (s ++ [v], env)
  v@(Str _)     -> (s ++ [v], env)
  v@(Decimal _) -> (s ++ [v], env)
  v@(Func _)    -> (s ++ [v], env)
  Id "clear"    -> ([], Map.empty)
  Id "def"      -> let [i, q, n] = take 3 (reverse s)
                   in (reverse . drop 3 $ reverse s,
                       Map.insert (unpackString n) (languageFunc q env, unpackNumber i) env)
  Id "call"     -> run (getBody (last s)) (init s) env
  Id v          -> (case Map.lookup v env of
                     Just res -> execute s v res
                     Nothing  -> [Error "Undefined function or name."], env)
  otherwise     -> ([Error "Unknown form or expression."], env)


run :: [YodaVal] -> [YodaVal] -> Env -> ([YodaVal], Env)
run [] s e = (s, e)
run exps stack env = let r = evalIdent env (head exps) stack
                     in case r of
                       ([Error v], e) -> ([Error v], e)
                       (s, e) -> run (tail exps) s e

eval str = run (parseAll str)
