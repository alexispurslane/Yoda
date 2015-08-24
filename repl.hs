module Main where

import Structures
import Parser
import Interpreter

import System.IO

import Data.List

repl s = do
  putStr ") "
  hFlush stdout -- Make sure the prompt shows first
  maybeLine <- getLine
  case maybeLine of
   "exit" -> return ()
   line -> do let ans = eval line s defaultEnv
              putStrLn $ "    " ++ case ans of
                [Error e] -> "ERROR: " ++ e
                v@_     -> intercalate "\n    " (map show v)
              repl ans

main = repl []
