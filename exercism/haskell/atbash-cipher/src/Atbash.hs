module Atbash (decode, encode) where

import Data.List (lookup, group, intercalate)
import Data.Char (toLower)

type KV = (Char, Char)

chunkOf :: Int -> [a] -> [[a]]
chunkOf _ [] = []
chunkOf n xs =
  let (ys, zs) = splitAt n xs
  in  ys : chunkOf n zs

codeSrc :: String
codeSrc = "abcdefghijklmnopqrstuvwxyz1234567890"

codeDesc :: String
codeDesc = "zyxwvutsrqponmlkjihgfedcba1234567890"

encodeTable :: [KV]
encodeTable = zip codeSrc codeDesc

decodeTable :: [KV]
decodeTable = zip codeDesc codeSrc

mapTable table str = lower >>= mapChar
  where
    lower = fmap toLower str
    mapChar ch = case lookup ch table of
      Nothing -> []
      Just ch -> [ch]

encode :: String -> String
encode str = intercalate " " grouped
  where
    grouped = chunkOf 5 mapped
    mapped = mapTable encodeTable str

decode :: String -> String
decode = mapTable decodeTable
