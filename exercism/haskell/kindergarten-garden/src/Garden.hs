module Garden
( Plant(..)
, defaultGarden
, garden
, lookupPlants
) where

import Data.List.Split (chunksOf)
import Data.Map (Map, findWithDefault, fromList)
import Data.List (sort, transpose)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Garden = Map String [Plant]

defaultGarden :: String -> Garden
defaultGarden = garden
  ["Alice",
   "Bob",
   "Charlie",
   "David",
   "Eve",
   "Fred",
   "Ginny",
   "Harriet",
   "Ileana",
   "Joseph",
   "Kincaid",
   "Larry"]

garden :: [String] -> String -> Garden
garden students plantList = fromList $ zip students' plantList'
  where
    plantList' = transform plantList
    students' = sort students
    transform = fmap mapPlant . matrix
    mapPlant = (fmap plant . concat)
    matrix = transpose . map (chunksOf 2) . lines

plant :: Char -> Plant
plant 'C' = Clover
plant 'G' = Grass
plant 'R' = Radishes
plant 'V' = Violets

lookupPlants :: String -> Garden -> [Plant]
lookupPlants = findWithDefault []
