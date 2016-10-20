module Roman (numerals) where

romanChar :: [(Int, String)]
romanChar = reverse $ [
      (1, "I"),
      (4, "IV"),
      (5, "V"),
      (9, "IX"),
      (10, "X"),
      (40, "XL"),
      (50, "L"),
      (90, "XC"),
      (100, "C"),
      (400, "CD"),
      (500, "D"),
      (900, "CM"),
      (1000, "M")
      ]

numerals :: Int -> Maybe String
numerals _n = Just $ numerals' romanChar _n []
  where
    numerals' :: [(Int, String)] -> Int -> String -> String
    numerals' ls remains acc
      | ls == [] = acc
      | remains <= 0 = acc
      | otherwise = numerals' xs r (acc ++ chars)
        where
        (n, str): xs = ls
        (d, r) = remains `divMod` n
        chars = foldl (++) "" (replicate d str)
