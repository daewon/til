module WordCount (wordCount) where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Char as C

validChars :: String
validChars = ['a'..'z'] ++ [' '] ++ ['1', '2']

filterValid :: String -> String
filterValid = filter (\a -> a `elem` validChars)

isSpace :: Char -> Bool
isSpace = (== ' ')

trim :: String -> String
trim = trimLeft . trimRight
  where
    trimLeft = dropWhile isSpace
    trimRight = reverse . trimLeft . reverse

splitText :: Char -> String -> [String]
splitText ch str = map trim $ L.groupBy (\_ b -> not $ b == ch) str

wordCount :: String -> M.Map String Int
wordCount s = M.fromListWith (+) validPairs
  where
    validPairs = filter (\(a, _) -> length a > 0) pairs
    pairs = fmap (\a -> (a, 1)) splited
    splited = splitText ' ' validChar
    validChar = filterValid normalized
    normalized = fmap C.toLower s
