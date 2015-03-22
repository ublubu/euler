module Euler7.Euler7
( main ) where

divisible :: Integer -> Integer -> Bool
divisible n r = n `mod` r == 0

concatPrimes :: [Integer] -> [Integer] -> [Integer]
concatPrimes ps [] = ps
concatPrimes ps (n:ns) = if any (\p -> divisible n p) ps
                         then concatPrimes ps ns
                         else concatPrimes (ps ++ [n]) ns

nPrimesImpl :: [Integer] -> Int -> Integer -> Integer -> [Integer]
nPrimesImpl ps n x blockSize
  | length ps >= n = ps
  | otherwise      = nPrimesImpl ps' n (x + blockSize + 1) blockSize
                     where ps' = (concatPrimes ps [x..x+blockSize]) 

nPrimes :: Int -> [Integer]
nPrimes n = take n $ nPrimesImpl [2, 3] n 4 100000

main :: IO ()
main = print $ last $ nPrimes 10001
