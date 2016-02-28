import Data.List (foldl')

fib1 :: Integer -> Integer
fib1 n = case n of 0 -> 0
                   1 -> 1
                   n -> fib1 (n - 1) + fib1 (n - 2)

listFib1 :: [Integer]
listFib1 = map fib1 [0..]

fib2 :: Integer -> Integer
fib2 n = listFib2 !! fromIntegral n

listFib2 :: [Integer]
listFib2 = [0, 1] ++ [listFib2 !! (n - 1) + listFib2 !! (n - 2) | n <- [2..]]

fact1 :: Integer -> Integer
fact1 n = case n of 0 -> 1
                    n -> n * fact1 (n - 1)

fact2 :: Integer -> Integer
fact2 n = product [1..n]

fact3 :: Integer -> Integer
fact3 n = foldl' (*) 1 [1..n]

fact4 :: [Integer]
fact4 = [1] ++ [(fromIntegral n) * (fact4 !! ((fromIntegral n) - 1)) | n <- [1..]]
