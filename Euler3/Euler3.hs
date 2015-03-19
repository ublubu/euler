module Euler3.Euler3
( main ) where

potentialFactors :: [Integer]
potentialFactors = iterate (+1) 2

data Result = Result { nextDivisor :: Integer, largestFactor :: Integer, remaining :: Integer } deriving (Show)

tryDivide :: Integer -> Integer -> Maybe Integer
tryDivide n divisor = if n `mod` divisor == 0
                      then Just $ quot n divisor
                      else Nothing

processResult :: Result -> Result
processResult x = let divisor = nextDivisor x
                      n = remaining x
                  in case tryDivide n divisor
                          of Just nextN -> Result { nextDivisor = divisor -- might need to divide by this again
                                                  , largestFactor = divisor
                                                  , remaining = nextN
                                                  }
                             Nothing -> Result { nextDivisor = divisor + 1
                                               , largestFactor = largestFactor x
                                               , remaining = n
                                               }

toResults :: Integer -> [Result]
toResults n = iterate processResult Result { nextDivisor = 2
                                           , largestFactor = 1
                                           , remaining = n
                                           }

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = largestFactor $ head $ filter (\r -> remaining r <= largestFactor r) $ toResults n

main :: IO()
main = print $ largestPrimeFactor 600851475143
