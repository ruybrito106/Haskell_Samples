q5 :: [Int] -> [Int]
q5 [] = []
q5 (x:list) = q5 small ++ (x : q5 large)
    where
        small = [y | y <- list, y <= x]
        large = [y | y <- list, y > x]