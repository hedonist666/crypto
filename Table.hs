module Table where
import Data.Map (Map, fromList, (!), adjust, toList)

list =[('a', 8.1),     ('k', 0.4),     ('v', 0.9),
       ('b', 1.4),     ('l', 3.4),     ('w', 1.5),
       ('c', 2.7),     ('m', 2.5),     ('x', 0.2),
       ('d', 3.9),     ('n', 7.2),     ('y', 1.9),
       ('e', 13.0),    ('o', 7.9),     ('z', 0.1),
       ('f', 2.9),     ('p', 2.0),         
       ('g', 2.0),     ('r', 6.9),         
       ('h', 5.2),     ('s', 6.1),         
       ('i', 6.5),     ('t', 10.5),        
       ('j', 0.2),     ('u', 2.4)]

dict = fromList list

freq :: String -> Map Char Double

freq str = fmap (\x -> x/26) $ helper str buf
    where buf             = fromList (zip ['a'..'z'] (cycle [0]))
          helper [] m     = m
          helper (c:cs) m = helper (cs) (adjust (+1) c m)
