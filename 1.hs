countDiv :: Int -> Int -> Int
countDiv n x
    | x == 1 = 1
    | (n `mod` x == 0) = 1 + countDiv (n) (x - 1)
    | otherwise = countDiv (n) (x - 1)

isPrime :: Int -> Bool
isPrime n = n /= 1 && countDiv n n <= 2

listPrimes :: [Int] -> [Int]
listPrimes list
    | list == [] = []
    | isPrime (head list) = (head list) : listPrimes (tail list)
    | otherwise = listPrimes (tail list)

boolPrimes :: [Int] -> [Bool]
boolPrimes list
    | list == [] = []
    | isPrime (head list) = True : boolPrimes (tail list)
    | otherwise = False : boolPrimes (tail list)

q1 :: [Int] -> ([Int], [Bool])
q1 list = (listPrimes list, boolPrimes list)