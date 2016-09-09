module Bob (responseFor) where
import Data.Char (isSpace, isUpper, isLetter)

isQuestion :: String -> Bool
isQuestion [] = False
isQuestion xs = lastChar == '?'
  where lastChar = head $ dropWhile isSpace $ reverse xs


isShout :: String -> Bool
isShout xs = and [hasAnyLetter, isAllUpper]
  where
    hasAnyLetter = any isLetter xs
    isAllUpper = all isUpper letters
    letters = filter isLetter xs


isNothing :: String -> Bool
isNothing = all isSpace

responseFor :: String -> String
responseFor xs
  | isNothing xs = "Fine. Be that way!"
  | isShout xs = "Whoa, chill out!"
  | isQuestion xs = "Sure."
  | otherwise = "Whatever."
