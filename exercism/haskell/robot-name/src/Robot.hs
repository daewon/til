module Robot (Robot, mkRobot, resetName, robotName) where
import System.Random

data Robot = Robot { name :: String } deriving Show

randomList :: (Random a) => Int -> [a]
randomList seed = randoms (mkStdGen seed)

mkRobot :: IO Robot
mkRobot = do
  gen <- newStdGen
  let ns = randoms gen :: [Int]
  return Robot { name = show ns }

resetName :: Robot -> IO ()
resetName = undefined


robotName :: Robot -> IO String
robotName = undefined
