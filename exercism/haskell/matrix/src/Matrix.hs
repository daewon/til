module Matrix
( Matrix
, cols
, column
, flatten
, fromList
, fromString
, reshape
, row
, rows
, shape
, transpose
) where

import qualified Data.Vector as V

data Matrix a = Matrix (V.Vector a) Int Int deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix _ _ c) = c

cells :: Matrix a -> V.Vector a
cells (Matrix v _ _)= v

rows :: Matrix a -> Int
rows (Matrix _ r _) = r

column :: Int -> Matrix a -> V.Vector a
column n m = V.ifilter inColumn $ flatten m
  where
    inColumn i _ = i `mod` c == n
    c = cols m

flatten :: Matrix a -> V.Vector a
flatten = cells

fromList :: [[a]] -> Matrix a
fromList xs = Matrix cells rows cols
  where
    cells = V.fromList (concat xs)
    rows = length xs
    cols = if null xs then 0 else length $ head xs

fromString :: (Read a) => String -> Matrix a
fromString = fromList . map convert . lines
  where
    convert line = if null r then [] else h : convert t
      where
        r = reads line
        (h, t) = head r

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) m = Matrix cells r c
  where
    cells = flatten m

row :: Int -> Matrix a -> V.Vector a
row n m = V.take c $ V.drop (n * c) (flatten m)
  where
    c = cols m

shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

transpose :: Matrix a -> Matrix a
transpose m = Matrix cells rows' cols'
  where
    cells = V.concat $ map (`column` m) [0 .. cols m - 1]
    rows' = cols m
    cols' = rows m
