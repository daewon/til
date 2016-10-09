module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Nil | Cons a (LinkedList a)

datum :: LinkedList a -> a
datum (Cons a _) = a
datum Nil = undefined

fromList :: [a] -> LinkedList a
fromList (x : xs) = Cons x $ fromList xs
fromList [] = Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new a xs = Cons a xs

next :: LinkedList a -> LinkedList a
next (Cons _ rest) = rest
next Nil = Nil

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList xs = revAcc xs Nil where
  revAcc :: LinkedList a -> LinkedList a -> LinkedList a
  revAcc (Cons a rest) acc = revAcc rest $ Cons a acc
  revAcc Nil acc = acc

toList :: LinkedList a -> [a]
toList (Cons a rest) = a : (toList rest)
toList Nil = []
