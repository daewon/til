module LeapYear (isLeapYear) where

evenly :: Integer -> Integer -> Bool
evenly year m = year `rem` m == 0

-- Leap Year
-- on every year that is evenly divisible by 4
-- except every year that is evenly divisible by 100
-- unless the year is also evenly divisible by 400
isLeapYear :: Integer -> Bool
isLeapYear y
  | evenly y 400 = True
  | evenly y 100 = False
  | evenly y 4 = True
  | otherwise = False
