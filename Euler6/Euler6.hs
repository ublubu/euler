module Euler6.Euler6
( main ) where

sumRange :: Integer -> Integer -> Integer
sumRange min max = let pairs = (max - min + 1) `quot` 2
                   in (min + max) * pairs

sumSquaresRange :: Integer -> Integer -> Integer
sumSquaresRange min max = sum $ map (\x -> x * x) [min..max]

difference :: Integer -> Integer -> Integer
difference min max = let sum = sumRange min max
                         sumSquares = sumSquaresRange min max
                     in sum * sum - sumSquares

main :: IO ()
main = print $ difference 1 100
