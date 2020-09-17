import Data.Char
main = do
    let s = cesar "test" 9
    putStrLn s
    putStrLn (dCesar s 9)
    

--encode
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
