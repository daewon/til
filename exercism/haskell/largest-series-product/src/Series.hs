module Series (largestProduct, windows, productLs) where

import Data.List (transpose, tails, elem)

toDigit :: Char -> Maybe Int
toDigit '0' = Just 0
toDigit '1' = Just 1
toDigit '2' = Just 2
toDigit '3' = Just 3
toDigit '4' = Just 4
toDigit '5' = Just 5
toDigit '6' = Just 6
toDigit '7' = Just 7
toDigit '8' = Just 8
toDigit '9' = Just 9
toDigit _ = Nothing

productLs :: String -> Maybe Int
productLs = fmap product . traverse toDigit

windows :: Int -> [a] -> [[a]]
windows n xs = filter pLenEq windows'
  where
    windows' = transpose (take n tails')
    tails' = tails xs
    pLenEq = (== n) . length

largestProduct :: Int -> String -> Maybe Int
largestProduct 0 _ = Just 1
largestProduct n str
  | null str || n < 0 || n > length str = Nothing
  | otherwise = fmap maximum $ traverse productLs lss
  where
    lss :: [String]
    lss = windows n str
