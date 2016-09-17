module Grains (square, total) where

import Data.Maybe (mapMaybe)

square :: Integral a => a -> Maybe a
square 1 = Just 1
square n
  | n <= 0 = Nothing
  | n > 64 = Nothing
  | otherwise = fmap (*2) $ square (n - 1)

total :: (Integral a) => a
total = sum $ mapMaybe square [1..64]
