module Phone (areaCode, number, prettyPrint) where

group :: Int -> [a] -> [[a]]
group _ [] = []
group n ls = take n ls : group n (drop n ls)

number :: String -> Maybe String
number ls = validDigit >>= validLen >>= validHead
  where
    validHead ls = case (length ls, head ls) of
      (11, '1') -> Just $ drop 1 ls
      (10, _) -> Just ls
      otherwise -> Nothing
    validLen digits = if length digits >= 10 then Just digits else Nothing
    validDigit = Just $ filter isValid ls
    isValid = flip elem "0123456789"

areaCode :: String -> Maybe String
areaCode s = take 3 <$> number s

prettyPrint :: String -> Maybe String
prettyPrint ls = do
  digits <- number ls
  area <- areaCode digits
  let
    al = length area
    mid = take al $ drop al digits
    post = drop al $ drop al digits
    in return $ "("++ area ++")" ++ " " ++ mid ++ "-" ++ post
