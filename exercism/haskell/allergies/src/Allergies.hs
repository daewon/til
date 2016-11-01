module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits

data Allergen = Cats
              | Chocolate
              | Eggs
              | Peanuts
              | Pollen
              | Shellfish
              | Strawberries
              | Tomatoes deriving (Eq)

value :: Allergen -> Int
value Eggs = 1
value Peanuts = 2
value Shellfish = 4
value Strawberries = 8
value Tomatoes = 16
value Chocolate = 32
value Pollen = 64
value Cats = 128

bitAnd :: Int -> Int -> Bool
bitAnd n m = (.&.) n m == m

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo alg n = bitAnd n $ value alg

allergies' :: [Allergen]
allergies' = [Cats, Chocolate, Eggs, Peanuts, Pollen, Shellfish, Strawberries, Tomatoes]

allergies :: Int -> [Allergen]
allergies n = filter (\a -> isAllergicTo a n) allergies'
