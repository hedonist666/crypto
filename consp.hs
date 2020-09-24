import Data.Char 
import Table
import qualified Data.Map as M
import System.IO

main = do 
    s <- inspectFile (cesar 3) "test" "cout"
    putStrLn ""
    putStrLn . show . crackCesar $ s
    s <- inspectFile (vigenere "lemon") "test" "cout"
    putStrLn . show . kasiski $ s

inspectFile :: (String -> String) -> String -> String -> IO String
inspectFile f ifn ofn 
    | ofn == "cout" && ifn == "cin" = getContents >>= \s -> putStr (f s) >> return (f s) 
    | ofn == "cout" = readFile ifn >>= \s -> putStr (f s) >> return (f s)
    | ifn == "cin"  = getContents >>= \s -> writeFile ofn (f s) >> return (f s)
    | otherwise = readFile ifn >>= \s -> writeFile ofn (f s) >> return (f s)

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

dVigenere :: [Char] -> [Char] -> [Char]
dVigenere ns xs = zipWith f cs xs
    where f n = chr . mOd . (subtract n) . ord 
          cs  = cycle (map ((subtract (ord 'a') ) . ord) ns)
          mOd c = ((c - ord 'a') `mod` 26) + ord 'a'

kasiski :: [Char] -> [Double]
kasiski s = helper s 1 []
    where l = fromIntegral (length s) :: Double
          ci s = 1/(l*l) * (li s) * (li s)
          li s = fromIntegral $ sum (map (\a -> count a s) ['a'..'z'])
          eq tol a b = tol > abs (a-b)
          count x = length . filter (==x)
          period [] t = []
          period (s:ss) t = s : period (drop (t - 1) ss) t
          helper s t buf | t > 26 = buf
                         | otherwise = helper s (t + 1) (ci (period s t) : buf)
                        -- | eq 5e-2 (ci (period s t)) 0.065 = t
                        -- | otherwise = helper s (t + 1)
