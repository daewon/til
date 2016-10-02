module School (School, add, empty, grade, sorted) where

import qualified Data.Map as M
import qualified Data.List as L

type Students = [String]
type School = M.Map Int Students

add :: Int -> String -> School -> School
add n name db = M.unionWith (++) db $ M.fromList [(n, [name])]

empty :: School
empty = M.empty

grade :: Int -> School -> Students
grade g db = case M.lookup g db of
  Just ls -> ls
  Nothing -> []

sorted :: School -> [(Int, Students)]
sorted m = fmap (\(n, ls) -> (n, L.sort ls)) schoolLs
  where schoolLs = M.toList m
