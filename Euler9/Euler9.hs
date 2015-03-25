module Euler9.Euler9
( main ) where

import Data.Monoid
import Control.Monad.Reader

tripletProduct :: (Integer, Integer, Integer) -> Integer
tripletProduct (a, b, c) = a * b * c

mPairToTriplet :: Reader Integer ((Integer, Integer) -> Maybe (Integer, Integer, Integer))
mPairToTriplet = do
  t <- ask
  return $ pairToTriplet t

pairToTriplet :: Integer -> (Integer, Integer) -> Maybe (Integer, Integer, Integer)
pairToTriplet total (a,b)
  | c <= b = Nothing
  | otherwise = if c ^ 2 == a ^ 2 + b ^ 2
                then Just (a, b, c)
                else Nothing
  where c = total - (a + b)

mTryTriplet :: Reader Integer ((Integer, Integer) -> (Integer, Integer, Integer, Bool))
mTryTriplet = do
  t <- ask
  return (\(a,b) -> let c = t - (a + b)
                        pyth = c ^ 2 == a ^ 2 + b ^ 2
                    in (a, b, c, c > b && pyth))

--tryTriplet :: Integer -> (Integer -> Integer) -> (Integer, Integer, Integer, Bool)

mPairsToTriplet :: Reader Integer ([(Integer, Integer)] -> Maybe (Integer, Integer, Integer))
mPairsToTriplet = do
  t <- ask
  return $ pairsToTriplet t

pairsToTriplet :: Integer -> [(Integer, Integer)] -> Maybe (Integer, Integer, Integer)
pairsToTriplet total ps = getFirst $ mconcat $ map (First . pairToTriplet total) ps

mPairsForTriplet :: Reader Integer (Integer -> [(Integer, Integer)])
mPairsForTriplet = do
  t <- ask
  return $ pairsForTriplet t

pairsForTriplet :: Integer -> Integer -> [(Integer, Integer)]
pairsForTriplet total a = map ((,) a) [a+1..total]
  --where bMax = a + (total - a) `quot` 2

mSeedsForTriplet :: Reader Integer [Integer]
mSeedsForTriplet = do
  t <- ask
  return [1..t `quot` 3]

main :: IO ()
main = print $  runReader (liftM (fmap tripletProduct) $ mPairsToTriplet `ap` liftM2 concatMap mPairsForTriplet mSeedsForTriplet) 1000
