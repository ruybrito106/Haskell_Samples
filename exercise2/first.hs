import Test.QuickCheck
import Control.Monad

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

delete :: Ord a => a -> Tree a -> Tree a
delete val (Node v t1 t2)
    | val < v     = Node v (delete val t1) t2
    | val > v     = Node v t1 (delete val t2)
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
    newt        = delete mini t2

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
prop_deletion = forAll (genTree 10 1 3) $ (\t -> elemT (constt) (delete constt (insTree constt t)) == False)