soma :: [Int] -> Int
soma lista
    | lista == [] = 0
    | otherwise = head (lista) + soma (tail (lista))

q7 :: [[Int]] -> [Int]
q7 lista
    | lista == [] = []
    | otherwise = (soma (head lista)) : q7 (tail (lista))