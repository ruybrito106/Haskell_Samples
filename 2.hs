countDiv :: Int -> Int -> Int
countDiv n x
    | x == 1 = 1
    | (n `mod` x == 0) = 1 + countDiv (n) (x - 1)
    | otherwise = countDiv (n) (x - 1)

isPrime :: Int -> Bool
isPrime n = n /= 1 && countDiv n n <= 2

q2 :: Int -> Int -> Int
q2 a b
    | b == a - 1 = 0
    | isPrime b = 1 + q2 (a) (b - 1)
    | otherwise = q2 (a) (b - 1)