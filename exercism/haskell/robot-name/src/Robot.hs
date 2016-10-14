module Robot (robotName, mkRobot, resetName) where

import System.Random
import Control.Concurrent (MVar, readMVar, swapMVar, newMVar)
import Control.Monad

data Robot = Robot (MVar String)

newName :: IO String
newName = sequence $ fmap randomRIO [chr, chr, ord, ord, ord]
  where (chr, ord) = (('A', 'Z'), ('0', '9'))

mkRobot :: IO Robot
mkRobot = newName >>= newMVar >>= return . Robot

robotName :: Robot -> IO String
robotName (Robot r) = readMVar r >>= return

resetName :: Robot -> IO ()
resetName (Robot r) = void (newName >>= swapMVar r)
