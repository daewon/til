module SecretHandshake (handshake) where

import Data.Bits
import Data.Maybe
import Data.Bool

class CanBit a where
  toBool :: a -> Maybe Bool

instance CanBit Char where
  toBool '0' = Just False
  toBool '1' = Just True
  toBool _ = Nothing

instance CanBit a => Handshake [a] where
  interpret = reverse . fromMaybe [] . traverse toBool

class Handshake a where
  interpret :: a -> [Bool]

instance Handshake Int where
  interpret n = map (testBit n) [0..numBits]
    where
      numBits = finiteBitSize n - countLeadingZeros n - 1

handshake :: Handshake a => a -> [String]
handshake = foldl (flip ($)) [] . zipWith (bool id) ops . interpret
  where
    ops = map snoc ["wink", "double blink", "close your eyes", "jump"] ++ [reverse]
    snoc s = (++ [s])
