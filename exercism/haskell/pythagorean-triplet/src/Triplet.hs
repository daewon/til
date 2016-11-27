module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

import Data.List (sort)

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a', b', c') = a * a + b * b == c * c
  where
    [a, b, c] = sort [a', b', c']

mkTriplet :: Int -> Int -> Int -> (Int, Int, Int)
mkTriplet a' b' c' = (a, b, c)
  where
    [a, b, c] = sort [a', b', c']

pythagoreanTriplets :: Int -> Int -> [(Int, Int, Int)]
pythagoreanTriplets min max =
  [mkTriplet a b c | a <- [min..max], b <- [a..max], c <- [b..max], isPythagorean (a, b, c)]
