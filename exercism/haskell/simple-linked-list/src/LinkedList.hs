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

fromList :: [a] -> LinkedList a
fromList (x : xs) = Cons x $ fromList xs
fromList [] = Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new a ls@(Cons _ _) = Cons a ls
new a Nil = Cons a Nil

next :: LinkedList a -> LinkedList a
next (Cons a rest) = rest
next Nil = Nil

nil :: LinkedList a
nil = Nil

rev (Cons a rest) acc = rev rest $ Cons a acc
rev Nil acc = acc

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList ls@(Cons a rest) = rev ls Nil
reverseLinkedList Nil = Nil

toList :: LinkedList a -> [a]
toList (Cons a rest) = a : (toList rest)
toList Nil = []
