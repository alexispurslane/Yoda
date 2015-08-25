module Main where

import Structures
import Parser
import Interpreter

import System.IO

import Data.List
import qualified Data.Map as Map

repl s = do
  putStr ") "
  hFlush stdout -- Make sure the prompt shows first
  maybeLine <- getLine
  case maybeLine of
   "exit" -> return ()
   line -> do let ans = uncurry (eval line) s
              putStrLn $ "    " ++ case ans of
                ([Error e], _) -> "ERROR: " ++ e
                (v, _)     -> intercalate "\n    " (map show v)
              repl ans

main = repl ([], defaultEnv)
