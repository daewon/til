module Triangle (rows) where

chunks :: Int -> [a] -> [[a]]
chunks n xs
  | length xs < n = []
  | otherwise = ys : chunks n zs
  where
    (ys, zs) = (take n xs, drop 1 xs)

group :: [a] -> [[a]]
group = chunks 2

row :: Int -> [Integer]
row 0 = []
row 1 = [1]
row 2 = [1, 1]
row n = [1] ++ current ++ [1]
  where
    current = fmap sum $ group prev
    prev = row $ n - 1

rows :: Int -> [[Integer]]
rows limit = [row n | n <- [1..limit]]
