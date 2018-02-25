import Prelude

monthCalculate :: Integer -> Integer
monthCalculate m
    | m <= 2 = m + 12
    | otherwise = m

dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = (d + t1 + k + t2 + t3 + 5 * j) `mod` 7
    where
        m = monthCalculate m

        j :: Integer
        j = y `div` 100

        k :: Integer
        k = y `mod` 100

        t1 :: Integer
        t1 = 13 * (m + 1) `div` 5

        t2 :: Integer
        t2 = k `div` 4

        t3 :: Integer
        t3 = j `div` 4