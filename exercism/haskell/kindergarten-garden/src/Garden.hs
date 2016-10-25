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

defaultGarden :: String -> Map String [Plant]
defaultGarden s = M.fromList pairs
  where
    pairs = partition s

garden :: [String] -> String -> Map String [Plant]
garden = undefined

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants n g = []

partition :: String -> [String]
partition ls = taked : partition rest
  where
    (taked, rest) = span (\c -> c /= '\n') dropped
    dropped = dropWhile (\c -> c /= '\n') ls
