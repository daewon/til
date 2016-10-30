module SecretHandshake (handshake) where

import Data.Bits

handshake :: Int -> [String]
handshake n = []
  where
    wink = ("wink", bit 1)
    doubleWink = ("doubleWink", bit 10)
    closeYourEyes = ("closeYourEyes", bit 100)
    jump = ("jump", bit 1000)
    bit = (.&.) n

-- 1 = wink
-- 10 = double blink
-- 100 = close your eyes
-- 1000 = jump

-- 10000 = Reverse the order of the operations in the secret handshake.
-- ```

-- Here's a couple of examples:

-- Given the input 9, the function would return the array
-- ["wink", "jump"]
