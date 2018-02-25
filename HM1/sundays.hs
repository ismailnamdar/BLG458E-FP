import Prelude

monthCalculate :: Integer -> Integer
monthCalculate m
    | m <= 2 = m + 12
    | otherwise = m

dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = (d + t1 + k + t2 + t3 + 5 * j) `mod` 7
    where

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

sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1
    where
        sundays' :: Integer -> Integer -> Integer
        sundays' y m
            | y > end = 0
            | otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest
            where
                nextY = if m > 12 then sundays' (y + 1) 1 else 0
                nextM = if m > 12 then 0 else sundays' y (m + 1)
                rest = nextY + nextM