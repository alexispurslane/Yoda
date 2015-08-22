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

 defaultEnv = Map.fromList [("+", numericBinop (+)),
                            ("-", numericBinop  (-)),
                            ("*", numericBinop (*)),
                            ("/", numericBinop div)]

 numericBinop :: (Int -> Int -> Int) -> [YodaVal] -> YodaVal
 numericBinop op params = (Number . foldl1 op) $ map unpackNumber params

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

 run :: [YodaVal] -> [YodaVal] -> Map.Map String ([YodaVal] -> YodaVal) -> [YodaVal]
 run exps stack env = foldr (eval env) stack exps
                      where eval env e s = case e of
                              v@(Number _)  -> v : s
                              v@(Str _)     -> v : s
                              v@(Decimal _) -> v : s
                              v@(Error _)   -> [v]
                              (Id v)        -> exec s (env Map.! v) 
                            exec s f = [Str "Not implemented. Sucking all arguments"]
