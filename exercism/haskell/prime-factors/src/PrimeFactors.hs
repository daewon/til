module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = factor n 2
  where
    factor :: Integer -> Integer -> [Integer]
    factor d m
      | d <= 1 = []
      | d `mod` m == 0 = m : factor (d `div` m) m
      | otherwise = factor d (m + 1)
