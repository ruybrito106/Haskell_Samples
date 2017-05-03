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

bookedByPerson :: DB -> Person -> [Book]
bookedByPerson b p = [l | (x, l) <- b, x == p]

bookedByBook :: DB -> Book -> [Person]
bookedByBook b bo = [p | (p, x) <- b, x == bo]

isBooked :: DB -> Book -> Bool
isBooked b bo = length (bookedByBook b bo) > 0

amountBookedByPerson :: DB -> Person -> Int
amountBookedByPerson b p = length (bookedByPerson b p)

book :: DB -> Person -> Book -> DB
book bo p b = (p, b):bo

unBook :: DB -> Person -> Book -> DB
unBook bo p b = [(a, c) | (a, c) <- bo, (a, c) /= (p, b)]

-- Membership of a list using list compression

membership2 :: [Int] -> Int -> Bool
membership2 list x = length ([y | y <- list, y == x]) > 0

-- Text processing samples

clearSpaces :: String -> String
clearSpaces str
    | str == [] = []
    | (head str) == ' ' = clearSpaces (tail str)
    | otherwise = (head str) : (clearSpaces (tail str))

-- Funções polimórficas

-- Reverse

rev [] = []
rev (a:as) = rev (as) ++ [a]

-- Size

myLength [] = 0
myLength (a:as) = 1 + myLength (as)

-- Take

takke 0 list = []
takke n (a:as) = a : takke (n - 1) (as)

-- Drrop

drrop 0 [] = []
drrop 0 (a:as) = a : (drrop 0 as)
drrop n (a:as) = drrop (n - 1) (as)

-- TakeWhile

takkeWhile _ [] = []
takkeWhile p (a:as)
    | p (a) = a : (takkeWhile p as)
    | otherwise = []

-- DropWhile

drropWhile _ [] = []
drropWhile p (a:as)
    | p (a) = (drropWhile p as)
    | otherwise = drop 1 as

-- Classes are collections of types for which there is an associated function
--      with different interpretations for each of them
--      e.g. Eq is the class for the function (==) which has different interpretations
--             for Int, String, Bool ...
--

-- Defining and creating instances for Eq

class MyEq t
    where
        (===) :: t -> t -> Bool

instance MyEq Bool
    where
        True === True = True
        False === False = True
        _ === _ = False

-- Defining and creating instances for Visible

class MyVisible t
    where
        myToString :: t -> String
        mySize :: t -> Int

instance MyVisible Char
    where
        myToString ch = [ch]
        mySize ch = 1

instance MyVisible Bool
    where
        myToString True = "True"
        myToString False = "False"
        mySize _ = 1

instance MyVisible t => MyVisible [t]
    where
        myToString = concat.(map myToString)
        mySize = (foldr (+) 0).(map mySize)

-- Using Eq

-- Only works for the types instantiated above
membership3 :: Eq t => [t] -> t -> Bool
membership3 [] x = False
membership3 (a:as) x = (a == x) || (membership3 as x)

-- Derived Classes

--class Eq t => Ord t
--    where
--        (<), (<=), (>), (>=) :: t -> t -> Bool
--        max, min :: t -> t -> t

data Season = Spring | Winter | Summer | Fall
data Weather = Cold | Hot

getWeather :: Season -> Weather
getWeather Winter = Cold
getWeather _ = Hot

-- Sample

data Shape =
    Circle Float
    | Rectangle Float Float

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

area :: Shape -> Float
area (Circle r) = 3.1415 * r * r
area (Rectangle l r) = l * r

-- Dias e aulas

data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo
data Aula = Metodologia | PLC | Projetao | HFC | Multimidia | Compiladores deriving (Show, Eq, Read)

aulas :: Dia -> [Aula]
aulas Segunda = [Projetao]
aulas Terca = [PLC, Compiladores]
aulas Quarta = [Metodologia, Multimidia]
aulas Quinta = [Metodologia, PLC]
aulas Sexta = [HFC, Compiladores]
aulas _ = []

isWeekend :: Dia -> Bool
isWeekend Sabado = True
isWeekend Domingo = True
isWeekend _ = False

hasPLC :: Dia -> Bool
hasPLC d = membership3 (aulas d) (PLC)

-- Recursive data

data Expr =
    Lit Int
    | Add Expr Expr
    | Sub Expr Expr
    deriving (Show)

eval :: Expr -> Int
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Sub x y) = (eval x) - (eval y)

showExpr :: Expr -> String
showExpr (Lit x) = show x
showExpr (Add x y) = (showExpr x) ++ "+" ++ (showExpr y)
showExpr (Sub x y) = (showExpr x) ++ "-" ++ (showExpr y)

-- Testing: eval (Add (Add (Lit 1) (Lit 10)) (Sub (Lit 2) (Lit 3)))
-- Polimorfic Data Assignment

data Pairs t = Pair t t
data List t = Nil | Cons t (List t) deriving (Show)
data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Show, Eq, Ord, Read)

-- Code a function to get the sum value of a tree
sumTree :: Tree Int -> Int
sumTree (NilT) = 0
sumTree (Node v e d) = v + sumTree (e) + sumTree (d)
-- Testing: sumTree (Node 2 (Node 3 (NilT) (NilT)) (Node 4 (NilT) (NilT)))
-- 9

-- Code a function to get the maximum value of a tree
maxTree :: Tree Int -> Int
maxTree (NilT) = 0
maxTree (Node v e d) = maxi (v) (maxi (maxTree (d)) (maxTree (e)))
-- Testing: maxTree (Node 2 (Node 3 (NilT) (NilT)) (Node 4 (NilT) (NilT)))
-- 4

-- Code a function to turn a List (data) into a List (predefined)
toList :: List t -> [t]
toList (Nil) = []
toList (Cons (a) (as)) = a : (toList (as))
-- Testing: toList (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Nil)))))
-- [2,3,4,5]

-- Code a function to turn a List (predfinfed) into a List (data)
fromList :: [t] -> List t
fromList [] = Nil
fromList (a:as) = Cons (a) (fromList (as))
-- Testing: toList ([2,3,4,5]) // Needs deriving Show
-- Cons 2 (Cons 3 (Cons 4 (Cons 5 (Nil))))

-- Code a function to calculate the depth of a tree
depth :: Tree t -> Int
depth (NilT) = 0
depth (Node n e d) = 1 + maxi (depth (e)) (depth (d))
-- Testing: depth (Node 2 (Node 3 (NilT) (NilT)) (Node 4 (Node 5 (NilT) (NilT)) (NilT)))
-- 3

-- Collapse a tree into an array
collapse :: Tree t -> [t]
collapse (NilT) = []
collapse (Node n e d) = (n : collapse (e)) ++ collapse (d)
-- Testing: collapse (Node 2 (Node 3 (NilT) (NilT)) (Node 4 (Node 5 (NilT) (NilT)) (NilT)))
-- [2,3,4,5]

-- BFS to find an element
bfs :: Eq t => Tree t -> t -> Bool
bfs (NilT) (_) = False
bfs (Node n e d) (x) = (x == n) || (bfs (e) (x)) || (bfs (d) (x))
-- Testing: bfs (Node 2 (Node 3 (NilT) (NilT)) (Node 4 (Node 5 (NilT) (NilT)) (NilT))) (5)
-- True

-- Map between two trees
mapTree :: (t -> u) -> Tree t -> Tree u
mapTree (f) (NilT) = NilT
mapTree (f) (Node n e d) = (Node (f (n)) (mapTree (f) (e)) (mapTree (f) (d)))
-- Testing: mapTree (>3) (Node 2 (Node 3 (NilT) (NilT)) (Node 4 (Node 5 (NilT) (NilT)) (NilT)))
-- Node False (Node False NilT NilT) (Node True (Node True NilT NilT) NilT)

-- DoubleList with map
double3 :: [Int] -> [Int]
double3 list = map (* 2) (list)

-- Take seconds with map
takeSeconds :: [(t, t)] -> [t]
takeSeconds list = map (snd) (list)

-- Map with list comprehension
myMap :: (t -> u) -> [t] -> [u]
myMap f list = [f (a) | a <- list]

-- Folding
fold :: (t -> t -> t) -> [t] -> t
fold f [a] = a
fold f (a:as) = f (a) (fold (f) (as))
-- defined as foldr1 in GHC

-- SumList with fold
sumList2 :: [Int] -> Int
sumList2 list = fold (+) list

-- Array and with fold
theAnd :: [Bool] -> Bool
theAnd list = fold (&&) list

-- Longest String
longestString :: [String] -> Int
longestString list = fold (max) (map (length) (list))

-- Insert element into tree
insertTree :: t -> Tree t -> Tree t
insertTree x tree = (Node x (tree) (NilT))

insertTree2 :: t -> Tree t -> Tree t
insertTree2 (x) (NilT) = (Node x (NilT) (NilT))
insertTree2 (x) (Node n e d) = Node n (insertTree2 (x) (e)) (d)

-- Testing:
--      insertTree (2) (Node 3 (NilT) (NilT))
--      Node 2 (Node 3 NilT NilT) NilT

--      insertTree2 (2) (Node 3 (NilT) (NilT))
--      Node 3 (Node 2 NilT NilT) NilT

createTree :: Ord t => [t] -> Tree t
createTree [] = (NilT)
createTree (a:as) = (Node a (createTree d) (createTree e))
    where
        d = [x | x <- as, x < a]
        e = [x | x <- as, x > a]

-- onlyPrimes with filter
onlyPrimes2 :: [Int] -> [Int]
onlyPrimes2 list = filter (isPrime) (list)

-- removerLowerSum
removerLowerSum :: [[Int]] -> Int -> [[Int]]
removerLowerSum list val = filter (func) (list)
    where
        func l = (foldr (+) 0 l) >= val