import Data.List

data Tree a = Empty | Nd a (Tree a) (Tree a) deriving (Show)

fromList :: Ord a => [a] -> Tree a
fromList [] = Empty
fromList (x:xs) = Nd x lhs rhs
  where
    lhs = fromList $ filter (< x) xs
    rhs = fromList $ filter (> x) xs

f :: IO String
f = do
  print "daewon"
  return "daewon"

with :: String -> IO String
with s = do
  print s
  return s

ask :: String -> IO String
-- ask is = is >>= return ""
ask is = return ""

main :: IO ()
main = f >>= ask >>= print
