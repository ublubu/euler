module Euler2.Euler2
( main ) where

fibs :: [Integer]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

relevantFibs :: [Integer]
relevantFibs = takeWhile (<= 4000000) fibs

isEven :: Integer -> Bool
isEven num = (num `mod` 2) == 0

evenRelevantFibs :: [Integer]
evenRelevantFibs = filter isEven relevantFibs

main :: IO()
main = print $ sum evenRelevantFibs
