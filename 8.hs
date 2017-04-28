fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

tribo :: Int -> Int
tribo 0 = 1
tribo 1 = 1
tribo 2 = 2
tribo n = tribo (n - 1) + tribo (n - 2) + tribo (n - 3)

lucas :: Int -> Int
lucas 0 = 2
lucas 1 = 1
lucas n = lucas (n - 1) + lucas (n - 2)

q8 :: Int -> Int
q8 n = fibo (n) + tribo (n) + lucas (n)