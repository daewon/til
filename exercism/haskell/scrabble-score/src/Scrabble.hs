module Scrabble (scoreLetter, scoreWord) where

import Data.Char
import qualified Data.Map as M

scoreMap :: M.Map Char Int
scoreMap = M.fromList $ lst >>= uncurry invert
  where
    invert n str = fmap (\ch -> (ch, n)) str
    lst = [
      (1, "AEIOULNRST"),
      (2, "DG"),
      (3, "BCMP"),
      (4, "FHVWY"),
      (5, "K"),
      (8, "JX"),
      (10,"QZ")]

scoreLetter :: Char -> Int
scoreLetter = findInScoreMap . toUpper
  where
    findInScoreMap = findWithDefault scoreMap
    findWithDefault = flip (M.findWithDefault 0)

scoreWord :: String -> Int
scoreWord = sum . fmap scoreLetter
