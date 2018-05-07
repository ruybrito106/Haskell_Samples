import Data.Char

fold :: (t -> t -> t) -> [t] -> t
fold f [a] = a
fold f (a:as) = f (a) (fold (f) (as))

myOrd :: Char -> Int
myOrd c = ord c - ord ('a') + 1

sumOrds :: [String] -> [Int]
sumOrds list = [fold (+) (map (myOrd) (str)) | str <- list]

-- Using filter
onlyDigits :: String -> String
onlyDigits str = filter (isDigit) (str)

onlyAlpha :: String -> String
onlyAlpha str = filter (isLetter) (str)

onlyPerfectSqr :: [Float] -> [Float]
onlyPerfectSqr list = filter (isPS) (list)
    where
        isPS x = (sqrt x) * (sqrt x) == x

-- String processing
--woords :: String -> [String]
--woords [] = []
--woords (a:as)
--    | isLetter a = (takeWhile (isLetter) (as)) ++ (woords (dropWhile (isLetter) (as)))
--    | otherwise = woords (dropWhile (== ' ') (as))

type Nome = String
type Idade = Int

data Pessoas = Pessoa Nome Idade deriving (Show)

showPerson :: Pessoas -> String
showPerson (Pessoa n a) = n ++ " -- " ++ show a

data Pilha t = Top | Cons t (Pilha t) deriving (Show)

push :: t -> Pilha t -> Pilha t
push x (Top) = (Cons x (Top))
push x (Cons c s) = (Cons c (push x s))

pop :: Pilha t -> Pilha t
pop Top = Top
pop (Cons c (Top)) = Top
pop (Cons c s) = (Cons c (pop s))

top :: Pilha t -> t
top (Cons c (Top)) = c
top (Cons c s) = top (s)

quick :: [Int] -> [Int]
quick [] = []
quick list = quick (lower) ++ ((head list) : quick (great))
    where
        lower = [x | x <- (tail list), x < (head list)]
        great = [x | x <- (tail list), x >= (head list)]