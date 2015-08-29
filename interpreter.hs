{-|
Module      : Interpreter
Description : Evaluates AST created from Parser module functions.
Copyright   : (c) Christopher Dumas, 2015
License     : GPL-3
Maintainer  : christopherdumas@gmail.com
Stability   : experimental
Portability : POSIX

This module contains all of the necissary functions and constants for evaluating the AST created by the
Parser.parseAll function.
-}
module Interpreter where

import Structures
import Parser

import qualified Data.Map as Map
import Data.Either
import Debug.Trace

-- | A type alias for a Haskell Map. The keys are strings, the value is a tuple containing the function, and its arity.
type Env = Map.Map String ([YodaVal] -> YodaVal, Int)

-- | Yoda built-in functions, or standard library.
defaultEnv :: Env
defaultEnv = Map.fromList [("+", (numericBinop (+), 2)),
                           ("-", (numericBinop (-), 2)),
                           ("*", (numericBinop (*), 2)),
                           ("/", (numericBinop div, 2)),
                           ("^", (numericBinop (^), 2)),
                           ("%", (numericBinop mod, 2)),
                           ("=", (yvalEqual, 2))]

-- | The equality function for Yoda.
yvalEqual :: [YodaVal] -> YodaVal
yvalEqual [Number x, y]  = Boolean $ x == unpackNumber y
yvalEqual [Str x, y]     = Boolean $ x == unpackString y
yvalEqual [Decimal x, y] = Boolean $ x == unpackDecimal y
yvalEqual [Error x, y]   = Boolean $ x == unpackString y
yvalEqual [Boolean x, y] = Boolean $ x == unpackBoolean y
yvalEqual [Func x, y]    = Boolean $ all (unpackBoolean . yvalEqual) [[x, y] | (x,y) <- (zip x (unpackFunc y))]

-- | Converts a Haskell function to a Yoda-executable function.
numericBinop :: (Int -> Int -> Int) -> [YodaVal] -> YodaVal
numericBinop op args = Number (unpackNumber (head args) `op` unpackNumber (head $ tail args))

-- | Executes a function
execute :: [YodaVal] -> String -> ([YodaVal] -> YodaVal, Int) -> [YodaVal]
execute s i f = case f of
  (fn, n)   -> if length s >= n
                  then (reverse . drop n $ reverse s) ++ [(fn . reverse) . take n $ reverse s]
                  else [Error "Data stack underflow."]

-- | Converts a Yoda lambda to a function that can be named and added to the 'defaultEnv'.
languageFunc :: YodaVal -> Env -> [YodaVal] -> YodaVal
languageFunc f e a = last (fst (run (getBody f) a e))

-- | Evaluates a single Yoda expression, with the environment and the stack.
evalExpr :: Env -> YodaVal -> [YodaVal] -> ([YodaVal], Env)
evalExpr env e s = case e of
  v@(Number _)  -> (s ++ [v], env)
  v@(Str _)     -> (s ++ [v], env)
  v@(Decimal _) -> (s ++ [v], env)
  v@(Func _)    -> (s ++ [v], env)
  v@(Boolean _) -> (s ++ [v], env)
  Id "clear"    -> ([], Map.empty)
  Id "def"      -> let [i, q, n] = take 3 (reverse s)
                   in (reverse . drop 3 $ reverse s,
                       Map.insert (unpackString n) (languageFunc q env, unpackNumber i) env)
  Id "call"     -> run (getBody (last s)) (init s) env
  Id v          -> (case Map.lookup v env of
                     Just res -> execute s v res
                     Nothing  -> [Error "Undefined function or name."], env)
  otherwise     -> ([Error "Unknown form or expression."], env)

-- | Evaluates multiple Yoda expressions.
run :: [YodaVal] -> [YodaVal] -> Env -> ([YodaVal], Env)
run [] s e = (s, e)
run exps stack env = let r = evalExpr env (head exps) stack
                     in case r of
                       ([Error v], e) -> ([Error v], e)
                       (s, e) -> run (tail exps) s e

-- | Evalutates a string.
eval str = run (parseAll str)
