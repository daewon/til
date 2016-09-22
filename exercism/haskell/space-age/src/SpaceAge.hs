module SpaceAge (Planet(..), ageOn) where

data Planet = Earth | Mercury | Venus | Mars | Jupiter | Saturn | Uranus | Neptune

earthSec :: Double
earthSec = 31557600.0

periodOf :: Planet -> Double
periodOf Earth = 1.0
periodOf Mercury = 0.2408467
periodOf Venus = 0.61519726
periodOf Mars = 1.8808158
periodOf Jupiter = 11.862615
periodOf Saturn = 29.447498
periodOf Uranus = 84.016846
periodOf Neptune = 164.79132

ageOn :: Planet -> Double -> Double
ageOn p i =  i / earthSec / periodOf p
