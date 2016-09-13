module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys = countM where
  countM = diffM >>= return . length
  diffM = case length xs == length ys of
    True -> Just $ filter neq $ zip xs ys where neq tpl = fst tpl /= snd tpl
    False -> Nothing
