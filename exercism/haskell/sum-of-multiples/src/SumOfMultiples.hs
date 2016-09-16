module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: Integral a => [a] -> a -> a
sumOfMultiples xs limit = sum fss
  where
    fss = nub $ xs >>= flsOfX
    flsOfX x = takeWhile (< limit) $ iterate (+x) x
