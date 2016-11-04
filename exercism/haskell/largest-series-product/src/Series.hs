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
windows n xs = filter (\a -> length a == n) $ transpose $ take n $ tails xs

largestProduct :: Int -> String -> Maybe Int
largestProduct n str
  | n == 0 = Just 1
  | null str = Nothing
  | n < 0 = Nothing
  | n > length str = Nothing
  | otherwise = fmap maximum $ traverse productLs lss
  where
    lss = windows n str
