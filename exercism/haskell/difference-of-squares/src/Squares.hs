module Squares (difference, squareOfSums, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOf - sumOf
  where
    sumOf = sumOfSquares n
    squareOf = squareOfSums n

squareOfSums :: Integral a => a -> a
squareOfSums n = (gaussSum n) ^ 2

sumOfSquares :: Integral a => a -> a
sumOfSquares n = n * (n + 1) * (2 * n + 1) `div` 6

square :: Integral a => a -> a
square = (^) 2

gaussSum :: Integral a => a -> a
gaussSum n = n * (n + 1) `div` 2
