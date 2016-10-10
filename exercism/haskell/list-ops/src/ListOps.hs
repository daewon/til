module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f nextVal xs
  where nextVal = f acc x

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc [] = acc
foldr f acc (x:xs) = f x nextVal
  where nextVal = foldr f acc xs

length :: [a] -> Int
length = foldl' (\a _ -> 1 + a) 0

reverse :: [a] -> [a]
reverse = foldl (\acc a -> a : acc) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x acc -> f x : acc) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldl' (\acc x -> next acc x) []
  where next acc x = case f x of
          True ->  x : acc
          _ ->  acc

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []
