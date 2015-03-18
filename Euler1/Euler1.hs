module Euler1.Euler1
( main ) where

multiples :: Integer -> [Integer]
multiples base = iterate (+ base) base

multiplesBelow :: Integer -> Integer -> [Integer]
multiplesBelow base limit = takeWhile (< limit) $ multiples base

totalBelow :: Integer -> Integer -> Integer
totalBelow base limit = sum $ multiplesBelow base limit

totalBelow' :: Integer -> Integer -> Integer
totalBelow' base limit =
  let count = quot (limit - 1) base
  in (* base) $ quot (count * (count + 1)) 2

main :: IO()
main = do print $ totalBelow 3 1000 + totalBelow 5 1000 - totalBelow 15 1000
          print $ totalBelow' 3 1000 + totalBelow' 5 1000 - totalBelow' 15 1000
