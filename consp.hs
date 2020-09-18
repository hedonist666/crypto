import Data.Char 
import Table
main = do 
    putStrLn "CESAR:"
    let s = cesar "test" 9 
    putStrLn s 
    putStrLn (dCesar s 9) 


cesar :: [Char] -> Int -> [Char] 
cesar [] _ = [] 
cesar (x:xs) n = f x : cesar xs n 
    where f = chr . mOd . (+n) . ord 
          mOd c = ((c - ord 'a') `mod` 26) + ord 'a'

dCesar :: [Char] -> Int -> [Char] 
dCesar [] _ = [] 
dCesar (x:xs) n = f x : dCesar xs n 
    where f = chr . mOd . (subtract n) . ord
          mOd c = ((c - ord 'a') `mod` 26) + ord 'a'

vigenere :: [Char] -> [Char] -> [Char]
vigenere xs ns = zipWith f cs xs
    where f n = chr . mOd . (+n) . ord 
          cs  = cycle (map ((subtract (ord 'a') ) . ord) ns)
          mOd c = ((c - ord 'a') `mod` 26) + ord 'a'
