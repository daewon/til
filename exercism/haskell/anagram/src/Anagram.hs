module Anagram (anagramsFor) where

import Data.List
import Data.Char

anagramsFor :: String -> [String] -> [String]

anagramsFor a [] = []
anagramsFor a (x:xs)
  | isAnagram && not identical = x : anagramsFor a xs
  | otherwise = anagramsFor a xs
  where
    isAnagram = sort upcaseA == (sort $ map toUpper x)
    identical = upcaseA == upcaseB
    upcaseA = upCase a
    upcaseB = upCase x
    upCase = map toUpper
