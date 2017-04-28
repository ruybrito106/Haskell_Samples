countDiv :: Int -> Int -> Int
countDiv n x
    | x == 1 = 1
    | (n `mod` x == 0) = 1 + countDiv (n) (x - 1)
    | otherwise = countDiv (n) (x - 1)

isPrime :: Int -> Bool
isPrime n = n /= 1 && countDiv n n <= 2

search :: Int -> Int -> (Int, Int)
search n x
    | x == 1 = (-1, -1)
    | isPrime (x) && isPrime (n - x) = (x, n - x)
    | otherwise = search (n) (x - 1)

q4 :: Int -> (Int, Int)
q4 n = search n n