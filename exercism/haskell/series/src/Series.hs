module Series (slices) where

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

slices :: Int -> String -> [[Int]]
slices 0 _ = [[]]
slices _ [] = []
slices n xs
  | n > length xs = []
  | otherwise = case nums of
      Just ls -> (take n ls) : (slices n $ drop 1 xs)
      Nothing -> []
  where
    nums = toInt xs
    toInt = traverse toDigit
