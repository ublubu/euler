module Euler4.Euler4
( main ) where

range :: [Integer]
range = reverse [100..999]

allProducts :: [Integer]
allProducts = [x*y | x <- range, y <- range]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) = (x == last xs) && (isPalindrome $ init xs)

reverseDigits :: Integer -> [Integer]
reverseDigits n = let lastDigit = n `mod` 10
                      shifted = n `quot` 10
                  in if shifted > 0
                     then lastDigit : reverseDigits shifted
                     else [lastDigit]

main :: IO()
main = print $ maximum $ filter (isPalindrome . reverseDigits) $ allProducts
