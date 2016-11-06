module Clock (clockHour, clockMin, fromHourMin, toString) where

data Clock = Clock { hour :: Int, min:: Int }

clockHour :: Clock -> Int
clockHour = undefined

clockMin :: Clock -> Int
clockMin = undefined

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = Clock h m

toString :: Clock -> String
toString (Clock h m) = "m"
