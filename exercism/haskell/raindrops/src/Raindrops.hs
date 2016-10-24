module Raindrops (convert) where

isDiviedBy :: Int -> Int -> Bool
isDiviedBy n m = n `mod` m == 0

toRainStr :: Int -> String
toRainStr 3 = "Pling"
toRainStr 5 = "Plang"
toRainStr 7 = "Plong"
toRainStr _ = ""

convert :: Int -> String
convert n = if joined == [] then show n else joined
  where
    joined = foldl (++) "" rainLs
    rainLs = fmap toRainStr valid
    valid = filter (isDiviedBy n) [3, 5, 7]
