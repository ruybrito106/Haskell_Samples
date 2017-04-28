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
l_pair (x) | fst (x) > snd (x) = fst (x)
            | otherwise = snd (x)

-- Implement maximum
maxi :: Int -> Int -> Int
maxi a b | a > b = a
            | otherwise = b

-- Find the largest value of a list
maxVal :: [Int] -> Int
maxVal x | x == [] = 0
               | head x > maxVal (tail x) = head x
               | otherwise = maxVal (tail x)

-- Find the largest tuple (means the product of members) of a list of tuples
maxValT :: [(Int, Int)] -> (Int, Int)
maxValT x | x == [] = (0, 0)
                | ( fst (head x) * snd (head x) ) > (fst (maxValT (tail x)) * snd (maxValT (tail x)) ) = head x
                | otherwise = maxValT (tail x)

prod :: (Int, Int) -> Int
prod (x) = fst (x) * snd (x)

simple_maxValT :: [(Int, Int)] -> (Int, Int)
simple_maxValT x | x == [] = (0, 0)
                            | prod (head x) > prod (simple_maxValT (tail x)) = head x
                            | otherwise = simple_maxValT (tail x)

-- Find the number of occurences of X in a list L
oc :: [Int] -> Int -> Int
oc l x | l == [] = 0
        | x == head l = 1 + oc (tail l) (x)
        | otherwise = oc (tail l) (x)