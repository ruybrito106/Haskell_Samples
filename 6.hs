q6 :: Int -> Int
q6 n
    | n == 0 = 0
    | otherwise = (n `mod` 10) + q6 (n `div` 10)