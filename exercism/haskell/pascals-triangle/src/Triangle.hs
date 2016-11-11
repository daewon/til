module Triangle (rows) where

rows :: Int -> [[Integer]]
rows limit
  | limit < 0 = []
  | otherwise = takeN triangles
  where
    triangles = iterate nextTriangle [1]
    takeN = take limit
    nextTriangle ls = fmap (uncurry (+)) $ zip (0 : ls) (ls ++[0])
