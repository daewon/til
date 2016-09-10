module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard f a =  keep (not . f) a

keep :: (a -> Bool) -> [a] -> [a]
keep f a = filter f a
