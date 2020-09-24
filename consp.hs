import Data.Char 
import Table
import qualified Data.Map as M
import System.IO

main = do 
    putStrLn "CESAR:"
    let s = cesar 9 "test" 
    putStrLn s 
    putStrLn (dCesar 9 s ) 

inspectFile :: (String -> String) -> String -> String -> IO() 
inspectFile f ifn ofn 
    | ofn == "cout" && ifn == "cin" = getContents >>= \s -> putStr (f s) 
    | ofn == "cout" = readFile ifn >>= \s -> putStr (f s)
    | ifn == "cin"  = getContents >>= \s -> writeFile ofn (f s)
    | otherwise = readFile ifn >>= \s -> writeFile ofn (f s)

cesar :: Int -> [Char] -> [Char] 
cesar _ []  = [] 
cesar n (x:xs) = f x : cesar n xs 
    where f = chr . mOd . (+n) . ord 
          mOd c = ((c - ord 'a') `mod` 26) + ord 'a'

dCesar :: Int -> [Char] -> [Char] 
dCesar _ []  = [] 
dCesar n (x:xs) = f x : dCesar n xs 
    where f = chr . mOd . (subtract n) . ord
          mOd c = ((c - ord 'a') `mod` 26) + ord 'a'

crackCesar :: [Char] -> Int
crackCesar str = dst (findBiggest . M.toList $ freq str) (findBiggest $ list)
    where findBiggest = foldl (\e acc -> if snd acc < snd e then e else acc) ('a', 0) 
          dst (a, v) (a1, v1) = (ord a) - (ord a1)

vigenere :: [Char] -> [Char] -> [Char]
vigenere ns xs = zipWith f cs xs
    where f n = chr . mOd . (+n) . ord 
          cs  = cycle (map ((subtract (ord 'a') ) . ord) ns)
          mOd c = ((c - ord 'a') `mod` 26) + ord 'a'
