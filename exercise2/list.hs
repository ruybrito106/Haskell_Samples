import Test.QuickCheck
import Control.Monad
import Data.List

-- Problem 1
-- Statement:
-- | Defina propriedades QuickCheck para testar a implementação de árvore de busca. É necessário definir um gerador.
-- Data definition (to be tested)

data Tree a = Nil | Node a (Tree a) (Tree a)   
    deriving (Eq, Show)                   

nil :: Tree a
nil = Nil

isNil :: Tree a -> Bool
isNil Nil = True
isNil _   = False

isNode :: Tree a -> Bool
isNode Nil = False 
isNode _   = True

leftSub :: Tree a -> Tree a
leftSub Nil            = error "leftSub"
leftSub (Node _ t1 _) = t1

rightSub :: Tree a -> Tree a
rightSub Nil            = error "rightSub"
rightSub (Node v t1 t2) = t2

treeVal  :: Tree a -> a
treeVal Nil            = error "treeVal"
treeVal (Node v _ _) = v

insTree :: Ord a => a -> Tree a -> Tree a
insTree val Nil = (Node val Nil Nil)
insTree val (Node v t1 t2)
    | v==val  = Node v t1 t2
    | val > v     = Node v t1 (insTree val t2)    
    | val < v     = Node v (insTree val t1) t2    

deleteNode :: Ord a => a -> Tree a -> Tree a
deleteNode val (Node v t1 t2)
    | val < v     = Node v (deleteNode val t1) t2
    | val > v     = Node v t1 (deleteNode val t2)
    | isNil t2    = t1
    | isNil t1    = t2
    | otherwise   = joinn t1 t2

minTree :: Ord a => Tree a -> Maybe a
minTree t
    | isNil t     = Nothing
    | isNil t1    = Just v
    | otherwise   = minTree t1
        where
        t1 = leftSub t
        v  = treeVal t

elemT :: Ord a => a -> Tree a -> Bool
elemT x Nil       = False
elemT x (Node y t1 t2)
    | x<y          = elemT x t1
    | x==y         = True
    | otherwise    = elemT x t2

joinn :: Ord a => Tree a -> Tree a -> Tree a
joinn t1 t2 
    = Node mini t1 newt
    where
    (Just mini) = minTree t2
    newt        = deleteNode mini t2

-- Generator definition

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized (arbitraryTree 2 1)

arbitraryTree _ _ 0 = return Nil
arbitraryTree a b n = frequency [
    (a, return Nil), 
    (b, liftM3 Node (arbitrary) (arbitraryTree a b (n `div` 2)) (arbitraryTree a b (n `div` 2))) ]

genTree :: Int -> Int -> Int -> Gen (Tree Int) 
genTree n a b = arbitraryTree a b n :: Gen (Tree Int)

-- Testing properties

prop_isNil :: Property
prop_isNil = forAll (genTree 10 1 0) $ (\t -> isNil t)

prop_isNode :: Property
prop_isNode = forAll (genTree 10 0 1) $ (\t -> isNode t)

prop_insertion :: Int -> Property
prop_insertion x = forAll (genTree 10 1 3) $ (\t -> elemT x (insTree x t))

constt :: Int -- Defined to avoid cases in which value deleted was present more than once on the Tree
constt = -2139021

prop_deletion :: Property
prop_deletion = forAll (genTree 10 1 3) $ (\t -> elemT (constt) (deleteNode constt (insTree constt t)) == False)











-- Problem 2
-- Statement:
-- | Defina propriedades QuickCheck para para o tipo Set que devem refletir as propriedades matemáticas de conjunto
-- Data definition (to be tested)

newtype Set a = Set [a] deriving (Show)

empty :: Set a
empty = Set []

isEmpty :: Eq a => Set a -> Bool
isEmpty s = eqSet s empty

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

elementSuchThat :: Set a -> (a -> Bool) -> a
elementSuchThat (Set []) f = error "elementSuchThat"
elementSuchThat (Set (x:xs)) f
    | f x == True = x
    | otherwise = elementSuchThat (Set xs) f

upsertSuchThat :: Ord a => Set a -> (a -> Bool) -> a -> Set a
upsertSuchThat (Set []) (f) (newElem) = sing newElem
upsertSuchThat (Set (x:xs)) (f) (newElem)
    | f x == True = unionn (sing newElem) (upsertSuchThat (Set xs) (f) (newElem))   
    | otherwise = unionn (sing x) (upsertSuchThat (Set xs) (f) (newElem))

unionn :: Ord a => Set a -> Set a -> Set a
unionn (Set xs) (Set ys) = Set (uni xs ys)

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
    arbitrary = liftM Set genOrdUniqueList 

genList :: (Arbitrary a) => Gen [a]
genList = frequency [ (1, return []), (9, liftM2 (:) arbitrary genList) ]

genOrdUniqueList :: (Arbitrary a, Ord a) => Gen [a]
genOrdUniqueList = genList >>= return . remDups . sort
    where 
        remDups [] = []
        remDups [x] = [x]
        remDups (x:y:xs)
            | x < y = x : remDups (y:xs)
            | otherwise = remDups (y:xs)

genSet :: Gen (Set Int) 
genSet = arbitrary :: Gen (Set Int)

genKSets :: Int -> Gen [Set Int]
genKSets k = vectorOf (k) (arbitrary :: Gen (Set Int))

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

prop_commutative_law :: Property
prop_commutative_law = forAll (genKSets 2) $ (
    \(a:b:as) -> 
        eqSet (unionn (a) (b)) (unionn (b) (a)) &&
        eqSet (inter (b) (a)) (inter (a) (b))
    )

prop_associative_law :: Property
prop_associative_law = forAll (genKSets 3) $ (
    \(a:b:c:as) -> 
        eqSet (unionn (unionn (a) (b)) (c)) (unionn (a) (unionn (b) (c))) &&
        eqSet (inter (inter (a) (b)) (c)) (inter (a) (inter (b) (c)))
    )

prop_distributive_law :: Property
prop_distributive_law = forAll (genKSets 3) $ (
    \(a:b:c:as) -> 
        eqSet (unionn (a) (inter (b) (c))) (inter (unionn (a) (b)) (unionn (a) (c))) &&
        eqSet (inter (a) (unionn (b) (c))) (unionn (inter (a) (b)) (inter (a) (c)))
    )

prop_identity_law :: Property
prop_identity_law = forAll (genSet) $ (\s -> eqSet (unionn (s) (empty)) (s))   











-- Problem 3
-- Statement:
-- | Utilize o TAD Set para implementar Store 
-- Data definition

type Var = Char
type Store = Set (Integer, Var)

initial :: Store
initial = Set []

value :: Store -> Var -> Integer
value (Set []) v = 0
value s v
    | isEmpty (filtered s) = 0
    | otherwise = fst (elementSuchThat (s) (\e -> snd e == v))
        where
            filtered s = filterSet (\e -> snd e == v) s

update :: Store -> Var -> Integer -> Store
update s key value = upsertSuchThat (s) (\e -> snd e == key) (value, key) 











-- Problem 4
-- Statement:
-- | A partir da implementação de árvore rubro-negra, verifique as invariantes através de propriedades QuickCheck
-- Data definition (to be tested)

data Color = R | B deriving (Eq, Show)

data RBTree a = E | T Color (RBTree a) a (RBTree a) deriving (Eq, Show)

-- Invariants
-- 1. No red node has a red parent
-- 2. Every path from the root node to an empty node contains the same number of black nodes
-- 3. The root and leaves of the tree are black

someNodeHasRedParent :: Eq a => RBTree a -> Bool
someNodeHasRedParent E = False
someNodeHasRedParent (T R left value right) 
    | left /= emptyRBTree || right /= emptyRBTree = False
    | otherwise = someNodeHasRedParent left || someNodeHasRedParent right
someNodeHasRedParent (T B left value right) = someNodeHasRedParent left || someNodeHasRedParent right

rootIsBlack :: RBTree a -> Bool
rootIsBlack E = True
rootIsBlack (T R _ _ _) = False
rootIsBlack (T B _ _ _) = True

leavesAreBlack :: Eq a => RBTree a -> Bool
leavesAreBlack E = True
leavesAreBlack (T R left _ right)
    | left == emptyRBTree && right == emptyRBTree = False
    | otherwise = (leavesAreBlack left) && (leavesAreBlack right)
leavesAreBlack (T B left _ right)
    | left == emptyRBTree && right == emptyRBTree = True
    | otherwise = (leavesAreBlack left) && (leavesAreBlack right)

rootAndLeavesAreBlack :: Eq a => RBTree a -> Bool
rootAndLeavesAreBlack a = (rootIsBlack a) && (leavesAreBlack a)

emptyRBTree :: RBTree a
emptyRBTree = E

isEmptyRBTree :: Eq a => RBTree a -> Bool
isEmptyRBTree x = x == emptyRBTree

member :: (Ord a) => a -> RBTree a -> Bool
member x E    = False
member x (T _ a y b)
    | x < y     = member x a
    | x == y    = True
    | otherwise = member x b

insertNode :: (Ord a) => a -> RBTree a -> RBTree a
insertNode x s = makeBlack $ ins s
    where 
        ins E  = T R E x E
        ins (T color a y b)
            | x < y  = balance color (ins a) y b
            | x == y = T color a y b
            | x > y  = balance color a y (ins b)
        makeBlack (T _ a y b) = T B a y b

balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b = T color a x b

balance' :: RBTree a -> RBTree a
balance' (T color left value right) = balance color left value right

delete :: (Ord a) => a -> RBTree a -> RBTree a
delete x t = makeBlack $ del x t
    where 
        makeBlack (T _ a y b) = T B a y b
        makeBlack E           = E

del :: (Ord a) => a -> RBTree a -> RBTree a
del x t@(T _ l y r)
    | x < y = delL x t
    | x > y = delR x t
    | otherwise = fuse l r

delL :: (Ord a) => a -> RBTree a -> RBTree a
delL x t@(T B t1 y t2) = balL $ T B (del x t1) y t2
delL x t@(T R t1 y t2) = T R (del x t1) y t2

balL :: RBTree a -> RBTree a
balL (T B (T R t1 x t2) y t3) = T R (T B t1 x t2) y t3
balL (T B t1 y (T B t2 z t3)) = balance' (T B t1 y (T R t2 z t3))
balL (T B t1 y (T R (T B t2 u t3) z t4@(T B l value r))) =
    T R (T B t1 y t2) u (balance' (T B t3 z (T R l value r)))

delR :: (Ord a) => a -> RBTree a -> RBTree a
delR x t@(T B t1 y t2) = balR $ T B t1 y (del x t2)
delR x t@(T R t1 y t2) = T R t1 y (del x t2)

balR :: RBTree a -> RBTree a
balR (T B t1 y (T R t2 x t3)) = T R t1 y (T B t2 x t3)
balR (T B (T B t1 z t2) y t3) = balance' (T B (T R t1 z t2) y t3) 
balR (T B (T R t1@(T B l value r) z (T B t2 u t3)) y t4) =
    T R (balance' (T B (T R l value r) z t2)) u (T B t3 y t4)

fuse :: RBTree a -> RBTree a -> RBTree a
fuse E t = t
fuse t E = t
fuse t1@(T B _ _ _) (T R t3 y t4) = T R (fuse t1 t3) y t4
fuse (T R t1 x t2) t3@(T B _ _ _) = T R t1 x (fuse t2 t3)
fuse (T R t1 x t2) (T R t3 y t4)  =
    let s = fuse t2 t3
    in case s of
        (T R s1 z s2) -> (T R (T R t1 x s1) z (T R s2 y t4))
        (T B _ _ _)   -> (T R t1 x (T R s y t4))
fuse (T B t1 x t2) (T B t3 y t4)  =
    let s = fuse t2 t3
    in case s of
        (T R s1 z s2) -> (T R (T B t1 x s1) z (T B s2 y t4))
        (T B s1 z s2) -> balL (T B t1 x (T B s y t4))

-- Generator definition

instance Arbitrary a => Arbitrary (RBTree a) where
    arbitrary = frequency [ 
        (3, return E), 
        (1, liftM4 T (arbitraryColor) (arbitraryRBTree) (arbitrary) (arbitraryRBTree)) ]

arbitraryColor :: Gen Color 
arbitraryColor = oneof [return R, return B]

arbitraryRBTree :: Arbitrary a => Gen (RBTree a)
arbitraryRBTree = frequency [
        (3, return E),
        (1, liftM4 T (arbitraryColor) (arbitraryRBTree) (arbitrary) (arbitraryRBTree)) ]

genRBTree :: Gen (RBTree Int)
genRBTree = arbitrary :: Gen (RBTree Int)

-- Testing Properties

prop_noNodeHasRedParent :: Property
prop_noNodeHasRedParent = forAll (genRBTree) $ (\t -> someNodeHasRedParent t == False)

prop_rootAndLeavesAreBlack :: Property
prop_rootAndLeavesAreBlack = forAll (genRBTree) $ (\t -> rootAndLeavesAreBlack t == True)