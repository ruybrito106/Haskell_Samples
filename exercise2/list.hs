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

instance  (Arbitrary a, Integral a) => Arbitrary (Tree a) where
    arbitrary = sized tree
  
tree :: (Integral b, Arbitrary b, Integral a) => b -> Gen (Tree a)
tree 0 = return Nil
tree n = frequency [(1, return Nil), (3, tree2 n 0 1000)]

tree2 :: (Integral b, Arbitrary b, Enum a, Integral a) => b -> a -> a -> Gen (Tree a)
tree2 0 _ _ = return Nil
tree2 n floor roof 
    | floor >= roof  = return Nil
    | otherwise = val >>= generate
    where 
        val = elements [floor..roof]
        subtree = tree2 (n `div` 2)
        generate = \x -> liftM2 (Node x) (subtree floor (x - 1)) (subtree (x + 1) roof)

genTree :: Int -> Gen (Tree Int)
genTree n = tree n :: Gen (Tree Int)

-- Testing auxiliary functions

isSearchTree :: Ord a => Tree a -> Bool
isSearchTree Nil = True
isSearchTree (Node x left right) = 
    ((isNil left) || x >= (treeVal left)) && 
    ((isNil right) || x <= (treeVal right)) &&
    (isSearchTree left) &&
    (isSearchTree right)

-- Testing properties

prop_isSearchTree :: Property
prop_isSearchTree = forAll (genTree 100) $ (\t -> isSearchTree t)










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

-- Testing auxiliary functions

isOrdered :: Ord a => Set a -> Bool
isOrdered (Set []) = True
isOrdered (Set [x]) = True
isOrdered (Set (x:y:xs)) = x <= y && isOrdered (Set (y:xs)) 

isUnique :: Ord a => Set a -> Bool
isUnique (Set []) = True
isUnique (Set [x]) = True
isUnique (Set (x:y:xs)) = x < y && isUnique (Set (y:xs)) 

-- Testing properties

prop_isOrdered :: Property
prop_isOrdered = forAll (genSet) $ (\s -> isOrdered s)

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

instance  (Arbitrary a, Integral a) => Arbitrary (RBTree a) where
    arbitrary = sized rbtree
  
rbtree :: (Integral b, Arbitrary b, Arbitrary a, Integral a) => b -> Gen (RBTree a)
rbtree 0 = return E
rbtree n = frequency [(1, return E), (3, rbtree2 n)]

rbtree2 :: (Integral b, Arbitrary b, Arbitrary a, Integral a) => b -> Gen (RBTree a)
rbtree2 0 = return E
rbtree2 n = val >>= generate
    where
        val = listOf (arbitrary)
        generate = \x -> return (rbtreeFromList x)

rbtreeFromList :: Ord a => [a] -> RBTree a 
rbtreeFromList [] = E
rbtreeFromList (x:xs) = insertNode (x) (rbtreeFromList xs)

genRBTree :: Int -> Gen (RBTree Int)
genRBTree n = rbtree n :: Gen (RBTree Int)

-- Testing auxiliary functions

isBlackOrEmpty :: Eq a => RBTree a -> Bool
isBlackOrEmpty E = True
isBlackOrEmpty (T B _ _ _) = True
isBlackOrEmpty (T R _ _ _) = False

isRedOrEmpty :: Eq a => RBTree a -> Bool
isRedOrEmpty E = True
isRedOrEmpty (T B _ _ _) = False
isRedOrEmpty (T R _ _ _) = True

redNodesHaveBothBlackChildren :: Eq a => RBTree a -> Bool
redNodesHaveBothBlackChildren E = True
redNodesHaveBothBlackChildren (T B left _ right) = 
    (redNodesHaveBothBlackChildren left) &&
    (redNodesHaveBothBlackChildren right) 
redNodesHaveBothBlackChildren (T R left val right) = 
    (isBlackOrEmpty left) &&
    (isBlackOrEmpty right) &&
    (redNodesHaveBothBlackChildren left) &&
    (redNodesHaveBothBlackChildren right) 

rootIsBlack :: RBTree a -> Bool
rootIsBlack E = True
rootIsBlack (T R _ _ _) = False
rootIsBlack (T B _ _ _) = True

leavesAreBlack :: Eq a => RBTree a -> Bool
leavesAreBlack _ = True -- Leaves are always uncolored in this structure

rootAndLeavesAreBlack :: Eq a => RBTree a -> Bool
rootAndLeavesAreBlack a = (rootIsBlack a) && (leavesAreBlack a)

-- Testing properties

prop_redNodesHaveBothBlackChildren :: Property
prop_redNodesHaveBothBlackChildren = forAll (genRBTree 100) $ (\t -> redNodesHaveBothBlackChildren t == True)

prop_rootAndLeavesAreBlack :: Property
prop_rootAndLeavesAreBlack = forAll (genRBTree 100) $ (\t -> rootAndLeavesAreBlack t == True)











-- Problem 5
-- Statement:
-- | Escreva uma propriedade que estabelece que uma árvore rubro-negra satisfaz a invariante de árvore binária de busca (ordem dos elementos)
-- Property definition

rbtreeVal :: RBTree a -> a
rbtreeVal E = error("treeVal")
rbtreeVal (T _ _ x _) = x

rbTreeIsSearchTree :: Ord a => RBTree a -> Bool
rbTreeIsSearchTree E = True
rbTreeIsSearchTree (T _ left x right) = 
    ((isEmptyRBTree left) || x >= (rbtreeVal left)) && 
    ((isEmptyRBTree right) || x <= (rbtreeVal right)) &&
    (rbTreeIsSearchTree left) &&
    (rbTreeIsSearchTree right)

prop_rbTreeIsSearchTree :: Property
prop_rbTreeIsSearchTree = forAll (genRBTree 100) $ (\t -> rbTreeIsSearchTree t)










-- Problem 6
-- Statement:
-- | Defina uma implementação do tipo abstrato de dados Set usando árvores de busca
-- Data definition