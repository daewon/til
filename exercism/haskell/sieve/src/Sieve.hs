module Sieve (primesUpTo) where

primesUpTo :: Int -> [Int]
primesUpTo 0 = []
primesUpTo 1 = []
primesUpTo l = takeWhile (<= l) $ primes 2
  where 
    primes n = n : next
      where
        next = filter (\a -> (a `rem` n) /= 0) $ primes (n + 1)
