module ETL (transform) where

import Data.Map (Map, toList, fromListWith)
import Data.Char (toLower)

transform :: Map Int String -> Map Char Int
transform from = fromListWith (+) flipped
  where
    flipped = lowerKvs >>= \(cnt, str) -> fmap (\ch -> (ch, cnt)) str
    lowerKvs = fmap (\(cnt, str) -> (cnt, fmap toLower str)) kvs
    kvs = toList from
