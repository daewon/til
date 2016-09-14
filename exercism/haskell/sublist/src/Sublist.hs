module Sublist (Sublist(..), sublist) where

-- The task is to create the data type `Sublist`, with `Eq` and
-- `Show` instances, and implement the function `sublist`.

import Data.List (isInfixOf)

data Sublist = Equal | Unequal | Sublist | Superlist deriving (Eq, Show)

sublist :: Eq a => [a] -> [a] -> Sublist
sublist as bs
  | as == bs = Equal
  | as `isInfixOf` bs = Sublist
  | bs `isInfixOf` as = Superlist
  | otherwise = Unequal
