import Data.Char
main = do
    let s = cesar "test" 9
    putStrLn s
    putStrLn (dCesar s 9)
    

--encode
cesar :: [Char] -> Int -> [Char]
cesar [] _ = [] -- для пустого массива пустой массив
cesar (x:xs) n = f x : cesar xs n -- отковыриваем элемент и делаем с ним вещи
    where f = chr . mOd . (+n) . ord
          mOd c = ((c - ord 'a') `mod` 26) + ord 'a'

--decode
dCesar :: [Char] -> Int -> [Char]
dCesar [] _ = [] -- для пустого массива пустой массив
dCesar (x:xs) n = f x : dCesar xs n -- отковыриваем элемент и делаем с ним вещи
    where f = chr . mOd . (subtract n) . ord
          mOd c = ((c - ord 'a') `mod` 26) + ord 'a'
