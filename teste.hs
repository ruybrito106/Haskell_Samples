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

-- Find out the N first even values of the fibonacci sequence
fibo :: Int -> Int
fibo n
    | n <= 0 = 1
    | otherwise = fibo (n - 1) + fibo (n - 2)

fibSeq :: Int -> Int -> [Int]
fibSeq n x
    | x == 0 = []
    | (fibo n) `mod` 2 == 0 = (fibo n) : (fibSeq (n + 1) (x - 1))
    | otherwise = (fibSeq (n + 1) (x))

firstN :: Int -> [Int]
firstN x = fibSeq (0) (x)

-- Sort based on sumDigits
sumDigits :: Int -> Int
sumDigits n
    | n == 0 = 0
    | otherwise = (n `mod` 10) + sumDigits (n `div` 10)

getList :: [Int] -> [Int]
getList list
    | list == [] = []
    | otherwise = (sumDigits (head list)) : (getList (tail list))

sortSum :: [Int] -> [Int]
sortSum [] = []
sortSum (x:list) = sortSum small ++ (sumDigits (x) : sortSum large)
    where
        small = [y | y <- getList (list), y <= sumDigits (x)]
        large = [y | y <- getList (list), y > sumDigits (x)]

-- Shift terms
shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((x, y), z) = (x, (y, z))

-- Solve two linear equations
oneRoot :: Float -> Float -> Float -> Float
oneRoot a b c = - b / (2 * a)

twoRoot :: Float -> Float -> Float -> (Float, Float)
twoRoot a b c = (d + e, d - e)
    where
        d = - b / (2 * a)
        e = sqrt (b*b - 4*a*c)

roots :: Float -> Float -> Float -> String
roots a b c
    | d == 0 = show (oneRoot a b c)
    | d > 0 = show (x) ++ " " ++ show (y)
    | otherwise = "no roots"
        where
            d = sqrt (b * b - 4 * a * c)
            (x, y) = twoRoot a b c

-- Implement lowerGreater

lower :: Int -> Int -> Int -> Int
lower a b c
    | a <= b && a <= c = a
    | b <= a && b <= c = b
    | c <= a && c <= b = c

greater :: Int -> Int -> Int -> Int
greater a b c
    | a >= b && a >= c = a
    | b >= a && b >= c = b
    | c >= a && c >= b = c

lowerGreater :: Int -> Int -> Int -> (Int, Int)
lowerGreater a b c = (lower a b c, greater a b c)

-- Implement orderTriple

orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (a, b, c) = (d, e, f)
    where
        d = lower a b c
        f = greater a b c
        e = a + b + c - d - f

-- Intersect lines
-- ERROR

type Point = (Float, Float)
type Line = (Point, Point)

--parallel :: Line -> Line -> Bool
--parallel (a, b) (c, d)
--    | a * d == b * c = True
--    | otherwise = False

-- First digit character of a string

isDigit :: Char -> Bool
isDigit a = (a >= 'a' && a <= 'z') || (a >= 'A' && a <= 'Z')

firstChar :: String -> Char
firstChar str
    | str == [] = '\0'
    | isDigit (head str) = (head str)
    | otherwise = firstChar (tail str)

onlyChar :: String -> String
onlyChar str
    | str == [] = []
    | isDigit (head str) = (head str) : onlyChar (tail str)
    | otherwise = onlyChar (tail str)

firstChar2 :: String -> Char
firstChar2 str = case (onlyChar str) of
    [] -> '\0'
    (a:as) -> a

-- Double List with list compression

double2 :: [Int] -> [Int]
double2 list = [2*x | x <- list]

-- Double if even

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

doubleEven :: [Int] -> [Int]
doubleEven list = [2*x | x <- list, isEven x]

-- sumPairs with list compression

sumPairs :: [(Int, Int)] -> [Int]
sumPairs list = [x+y | (x, y) <- list]

-- OnlyChar with list compression

onlyChar2 :: String -> String
onlyChar2 str = [x | x <- str, isDigit x]

-- Implement a sample of a DB for people and books

type Person = String
type Book = String
type DB = [(Person, Book)]

banco :: DB
banco = [("Joao", "A"),
               ("Maria", "B"),
               ("Victor", "C"),
               ("Luis", "A"),
               ("Maria", "D")]

--bookedByPerson :: DB -> Person -> [Book]
--bookedByPerson b p = [l | (p, l) <- b]