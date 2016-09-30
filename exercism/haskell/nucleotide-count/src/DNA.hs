module DNA (count, nucleotideCounts) where

import qualified Data.Map as M

nucleotides :: String
nucleotides = "ACGT"

nucleotideMap :: M.Map Char Char
nucleotideMap = M.fromList $ zip nucleotides nucleotides

count :: Char -> String -> Either String Int
count ch str = case (isValid, elem ch nucleotides) of
  (Just ls, True) -> Right $ countElem ch ls
  _ -> Left "Invalid"
  where
    isValid :: Maybe [Char]
    isValid = traverse find str
    find :: Char -> Maybe Char
    find = flip M.lookup $ nucleotideMap

countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)

nucleotideCounts :: String -> Either String (M.Map Char Int)
nucleotideCounts str = nucleotideCountsWith str $ all (flip elem nucleotides) str

nucleotideCountsWith :: String -> Bool -> Either String (M.Map Char Int)
nucleotideCountsWith str False = Left $ "Invalid input: " ++ str
nucleotideCountsWith str _ =
  Right $  M.unionWith (+) baseM countM
    where
      countM = M.fromListWith (+) $ fmap toKv str
      toKv c = (c, 1)
      baseM = fmap (\_ -> 0) nucleotideMap
