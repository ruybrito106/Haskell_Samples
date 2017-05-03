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