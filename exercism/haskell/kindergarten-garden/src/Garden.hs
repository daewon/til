module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import qualified Data.Map as M
import Data.Map (Map)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Person = Alice
            | Bob
            | Charlie
            | David
            | Eve
            | Fred
            | Ginny
            | Harriet
            | Ileana
            | Joseph
            | Kincaid
            | Larry
            deriving (Eq, Show, Ord, Enum)

flowerMap :: Map Char Plant
flowerMap = M.fromList [('C', Clover), ('G', Grass), ('R', Radishes), ('V', Violets)]

defaultGarden :: String -> Map String [Plant]
defaultGarden s = M.empty

garden :: [String] -> String -> Map String [Plant]
garden = undefined

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants str g = line >>= (\l -> fmap (\ch -> Clover) l)
  where line = partition str

partition :: String -> [String]
partition [] = []
partition ls = taked : partition rest
  where
    (taked, rest) = span (\c -> c /= '\n') dropped
    dropped = dropWhile (\c -> c == '\n') ls

trim :: String -> String
trim str = trimed
  where
    trimed = reverse $ reverse (trimLeft str)
    trimLeft str = dropWhile (== '\n')
