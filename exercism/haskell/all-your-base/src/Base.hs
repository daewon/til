module Base (rebase) where

toBaseDigits :: Integral a => a -> a -> [a]
toBaseDigits base = reverse . toBaseDigits'
  where
    toBaseDigits' n = if d > 0 then m : toBaseDigits' d else [m]
      where (d, m) = n `divMod` base

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits
  | inputBase < 2 || outputBase < 2 = Nothing
  | any (< 0) inputDigits = Nothing
  | any (>= inputBase) inputDigits = Nothing
  | dropWhile (== 0) inputDigits == [] = Just []
  | otherwise = Just $ toBaseDigits outputBase decimal
  where
    decimal = foldl (+) 0 digitsWithBase
      where
        digitsWithBase = fmap tupleMul $ zip digits $ iterate (* inputBase) 1
        tupleMul = uncurry (*)
        digits = reverse inputDigits
