module School (School, add, empty, grade, sorted) where

import qualified Data.Map as M
import qualified Data.List as L

type Grade = Int
type Student = String
type Students = [Student]
type School = M.Map Int Students

add :: Grade -> String -> School -> School
add n name db = M.unionWith (++) db $ M.fromList [(n, [name])]

empty :: School
empty = M.empty

grade :: Grade -> School -> Students
grade g db = case M.lookup g db of
  Just ls -> ls
  Nothing -> []

sorted :: School -> [(Grade, Students)]
sorted m = fmap sortStudents schoolLs
  where
    schoolLs = M.toList m
    sortStudents (n, ls) = (n, L.sort ls)
