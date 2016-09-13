module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard f =  keep $ not . f

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep f (x:xs) = if f x then x:keep f xs else keep f xs
