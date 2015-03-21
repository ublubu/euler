module Euler5.Euler5
( main ) where

removeMultiples :: Integer -> [Integer] -> [Integer]
removeMultiples r = filter (\n -> n `mod` r /= 0)

primes :: [Integer] -> [Integer]
primes [] = []
primes (x:[]) = [x]
primes (x:xs) = x : primes (removeMultiples x xs)

primesUntil :: Integer -> [Integer]
primesUntil r = primes [2..r]

powerify :: Integer -> Integer -> Integer
powerify base = powerifyHelper base base

powerifyHelper :: Integer -> Integer -> Integer -> Integer
powerifyHelper base x max = let y = x * base
                            in if y > max
                               then x
                               else powerifyHelper base y max

poweredPrimes :: Integer -> [Integer]
poweredPrimes max = map (\r -> powerify r max) $ primesUntil max

main :: IO()
main = print $ product $ poweredPrimes 20 
