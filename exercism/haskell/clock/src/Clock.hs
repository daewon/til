module Clock (clockHour, clockMin, fromHourMin, toString) where

import Text.Printf

data Clock = Clock Int deriving (Show, Eq)

instance Num Clock where
  (Clock x) + (Clock y) = Clock (x+y)
  (Clock x) - (Clock y) = Clock ((x-y) `mod` (60*24))
  x * y = id 1
  abs = id
  signum = id
  fromInteger = Clock . fromInteger

clockHour :: Clock -> Int
clockHour (Clock min) = min `div` 60

clockMin :: Clock -> Int
clockMin (Clock min) = min `mod` 60

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = Clock (60 * hour + min)
  where
    hour = (h + m `div` 60) `mod` 24
    min = m `mod` 60

toString :: Clock -> String
toString c = printf "%02d:%02d" hour min
  where
    hour = clockHour c
    min = clockMin c
