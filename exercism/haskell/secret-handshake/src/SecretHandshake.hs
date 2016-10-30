module SecretHandshake (handshake) where

import Data.Bits
import Data.Maybe
import Data.Bool

class Bitlike a where
  toBit :: a -> Maybe Bool

instance Bitlike Char where
  toBit '0' = Just False
  toBit '1' = Just True
  toBit _   = Nothing

class Handshake a where
  interpret :: a -> [Bool]

instance Handshake Int where
  interpret n = map (testBit n) [0 .. numBits]
    where numBits = finiteBitSize n - countLeadingZeros n - 1

instance Bitlike a => Handshake [a] where
  interpret = reverse . fromMaybe [] . traverse toBit

handshake :: Handshake a => a -> [String]
handshake = foldl (flip ($)) [] . zipWith (bool id) ops . interpret
  where
    ops = map snoc ["wink", "double blink", "close your eyes", "jump"] ++ [reverse]
    snoc s = (++ [s])
