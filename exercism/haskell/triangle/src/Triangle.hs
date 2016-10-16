module Triangle (TriangleType(..), triangleType) where

import Data.List (sort)

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: Float -> Float -> Float -> TriangleType
triangleType a b c = triangleTypeInner $ sort [a, b, c]
  where
    triangleTypeInner :: [Float] -> TriangleType
    triangleTypeInner (a:b:c:[])
      | a + b <= c = Illegal
      | (a == b) && (b == c) = Equilateral
      | a == b || b == c = Isosceles
      | otherwise = Scalene
