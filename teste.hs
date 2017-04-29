-- Exemplos em haskell
-- Prova 01 - Paradigmas de Linguagens Computacionais

-- Testando o ghci
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

main = print (fac 20)

-- Tutorial
-- Tuples

-- Find the largest item of a pair
l_pair :: (Int, Int) -> Int
l_pair (x)
    | fst (x) > snd (x) = fst (x)
    | otherwise = snd (x)

-- Implement maximum
maxi :: Int -> Int -> Int
maxi a b
    | a > b = a
    | otherwise = b

-- Find the largest value of a list
maxVal :: [Int] -> Int
maxVal x
    | x == [] = 0
    | head x > maxVal (tail x) = head x
    | otherwise = maxVal (tail x)

-- Find the largest tuple (means the product of members) of a list of tuples
maxValT :: [(Int, Int)] -> (Int, Int)
maxValT x
    | x == [] = (0, 0)
    | ( fst (head x) * snd (head x) ) > (fst (maxValT (tail x)) * snd (maxValT (tail x)) ) = head x
    | otherwise = maxValT (tail x)

prod :: (Int, Int) -> Int
prod (x) = fst (x) * snd (x)

simple_maxValT :: [(Int, Int)] -> (Int, Int)
simple_maxValT x
    | x == [] = (0, 0)
    | prod (head x) > prod (simple_maxValT (tail x)) = head x
    | otherwise = simple_maxValT (tail x)

-- Find the number of occurences of X in a list L
oc :: [Int] -> Int -> Int
oc l x
    | l == [] = 0
    | x == head l = 1 + oc (tail l) (x)
    | otherwise = oc (tail l) (x)

-- Find the prod from the list of lists of int with the largest number of items
prodL :: [Int] -> Int
prodL l
    | l == [] = 1
    | otherwise = (head l) * prodL (tail l)

size :: [Int] -> Int
size x
    | x == [] = 0
    | otherwise = 1 + size (tail x)

ssize :: [[Int]] -> Int
ssize x
    | x == [] = 0
    | otherwise = 1 + ssize (tail x)

prodList :: [[Int]] -> [Int]
prodList x
    | x == [] = []
    | size (head x) > size (prodList (tail x)) = head x
    | otherwise = prodList (tail x)

-- [1, 2] /= [2, 1]

-- No declaration to guarantee covering all types
listEqual x y = x == y

-- Find out whether a number is prime or not
countDiv :: Int -> Int -> Int
countDiv n x
    | x == 1 = 1
    | (n `mod` x == 0) = 1 + countDiv (n) (x - 1)
    | otherwise = countDiv (n) (x - 1)

isPrime :: Int -> Bool
isPrime n = n /= 1 && countDiv n n <= 2

-- Code a function to make from a list another one with doubled values
double :: [Int] -> [Int]
double list
    | list == [] = []
    | otherwise = (2 * (head list)) : double (tail (list))

-- Code a function to verify membership of an element from a list of integers
member :: [Int] -> Int -> Bool
member list x
    | list == [] = False
    | otherwise = (x == head list) || member (tail list) (x)

-- Code a function to return a string with the numbers within another string
isNum :: Char -> Bool
isNum x = x >= '0' && x <= '9'

clear :: String -> String
clear str
    | str == [] = []
    | isNum (head str) = (head str) : clear (tail str)
    | otherwise = clear (tail str)

-- Code a function that returns the sum of the elements from two lists
sumPair :: [Int] -> [Int] -> [Int]
sumPair l1 l2
    | l1 == [] = []
    | otherwise = ((head l1) + (head l2)) : (sumPair (tail l1) (tail l2))

-- Code a quickSort
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:list) = quickSort small ++ (x : quickSort large)
    where
        small = [y | y <- list, y <= x]
        large = [y | y <- list, y > x]