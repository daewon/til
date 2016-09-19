module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance [] _ = Nothing
distance _ [] = Nothing
distance (x:xs) (y:ys) = if eq then rest else rest >>= return . (+1)
  where
    eq = x == y
    rest = distance xs ys
