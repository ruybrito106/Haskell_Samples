import Test.QuickCheck

data Colour = Red | Green | Blue 
    deriving (Show)

instance Arbitrary Colour where
    arbitrary = oneof [return Red, return Green, return Blue]

data Ternary = Yes | No | Unknown 
    deriving (Eq, Show)

instance Arbitrary Ternary where
    arbitrary = do 
        n <- choose (0, 10) :: Gen Int
        return $ case n of
            0 -> Yes
            1 -> No
            _ -> Unknown 