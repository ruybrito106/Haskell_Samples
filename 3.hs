q3 :: [Int] -> Int -> Int
q3 lista x
    | x > length (lista) = -1
    | x == 1 = head lista
    | otherwise = q3 (tail lista) (x - 1)