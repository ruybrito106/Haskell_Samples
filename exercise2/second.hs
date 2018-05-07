import Data.List
import Test.QuickCheck
import Control.Monad

-- Data definition (to be tested)

newtype Set a = Set [a] deriving (Show)

empty :: Set a
empty = Set []

instance Eq a => Eq ( Set a ) where
    (==) = eqSet

eqSet :: Eq a => Set a -> Set a -> Bool
eqSet (Set xs) (Set ys) = (xs == ys)

instance Ord a => Ord ( Set a ) where
    (<=) = leqSet

leqSet :: Ord a => Set a -> Set a -> Bool
leqSet (Set xs) (Set ys) = (xs <= ys)

sing :: a -> Set a
sing x = Set [x]

memSet :: Ord a => Set a -> a -> Bool
memSet (Set []) y = False
memSet (Set (x:xs)) y
    | x < y = memSet (Set xs) y
    | x == y = True
    | otherwise = False

union :: Ord a => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (uni xs ys)

uni :: Ord a => [a] -> [a] -> [a]
uni [] ys = ys
uni xs [] = xs
uni (x:xs) (y:ys)
    | x < y = x : uni xs (y:ys)
    | x == y = x : uni xs ys
    | otherwise = y : uni (x:xs) ys

inter :: Ord a => Set a -> Set a -> Set a
inter (Set xs) (Set ys) = Set (int xs ys)

int :: Ord a => [a] -> [a] -> [a]
int [] ys = []
int xs [] = []
int (x:xs) (y:ys)
    | x < y = int xs (y:ys)
    | x == y = x : int xs ys
    | otherwise = int (x:xs) ys

diff :: Ord a => Set a -> Set a -> Set a
diff (Set xs) (Set ys) = Set (dif xs ys)

dif :: Ord a => [a] -> [a] -> [a]
dif [] ys = []
dif xs [] = xs
dif (x:xs) (y:ys)
    | x < y = x : dif xs (y:ys)
    | x == y = dif xs ys
    | otherwise = dif (x:xs) ys

subSet :: Ord a => Set a -> Set a -> Bool
subSet (Set xs) (Set ys) = subS xs ys

subS :: Ord a => [a] -> [a] -> Bool
subS [] ys = True
subS xs [] = False
subS (x:xs) (y:ys)
    | x < y = False
    | x == y = subS xs ys
    | x > y = subS (x:xs) ys

makeSet :: Ord a => [a] -> Set a
makeSet = Set . remDups . sort
    where
    remDups [] = []
    remDups [x] = [x]
    remDups (x:y:xs)
        | x < y = x : remDups (y:xs)
        | otherwise = remDups (y:xs)

mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet f (Set xs) = makeSet (map f xs)

filterSet :: (a -> Bool) -> Set a -> Set a
filterSet p (Set xs) = Set (filter p xs)

foldSet :: (a -> a -> a) -> a -> Set a -> a
foldSet f x (Set xs) = (foldr f x xs)

showSet :: (a -> String) -> Set a -> String
showSet f (Set xs) = concat (map (( ++ "\n" ) . f) xs)

card :: Set a -> Int
card (Set xs) = length xs

flatten :: Set a -> [a]
flatten (Set xs) = xs

-- Generator definition

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = liftM Set genOrdList 

genList :: (Arbitrary a) => Gen [a]
genList = frequency [ (1, return []), (9, liftM2 (:) arbitrary genList) ]

genOrdList :: (Arbitrary a, Ord a) => Gen [a]
genOrdList = genList >>= return . sort

genSet :: Gen (Set [Int]) 
genSet = arbitrary :: Gen (Set [Int])

-- Testing properties

isOrdered :: Ord a => Set a -> Bool
isOrdered (Set []) = True
isOrdered (Set [x]) = True
isOrdered (Set (x:y:xs)) = x <= y && isOrdered (Set (y:xs)) 

prop_isOrdered :: Property
prop_isOrdered = forAll (genSet) $ (\s -> isOrdered s)

isUnique :: Ord a => Set a -> Bool
isUnique (Set []) = True
isUnique (Set [x]) = True
isUnique (Set (x:y:xs)) = x < y && isUnique (Set (y:xs)) 

prop_isUnique :: Property
prop_isUnique = forAll (genSet) $ (\s -> isUnique s)

