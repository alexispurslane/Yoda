module Main where

import Structures
import Parser
import Interpreter

import System.IO
import System.Console.Readline

import Data.List
import qualified Data.Map as Map

repl s = do
  maybeLine <- readline ":-) "
  case maybeLine of
    Nothing      -> return ()
    Just "exit"  -> return ()
    Just "quit"  -> return ()
    Just line    -> do addHistory line
                       let ans = uncurry (eval line) s
                       putStrLn $ "    " ++ case ans of
                         ([Error e], _) -> "ERROR: " ++ e
                         (v, _)     -> intercalate "\n    " (map show v)
                       repl ans

main = repl ([], defaultEnv)
