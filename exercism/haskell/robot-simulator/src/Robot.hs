module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum)

data Robot = Robot { dir :: Bearing, pos :: (Integer, Integer) }

bearing :: Robot -> Bearing
bearing (Robot dir _) = dir

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ pos) = pos

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate r@(Robot bear pos) cmds = foldl simulate' r cmds
  where
    simulate' r@(Robot North (x, y)) 'A' = r { pos = (x, succ y) }
    simulate' r@(Robot South (x, y)) 'A' = r { pos = (x, pred y) }
    simulate' r@(Robot East (x, y)) 'A' = r { pos = (succ x, y) }
    simulate' r@(Robot West (x, y)) 'A' = r { pos = (pred x, y) }
    simulate' r@(Robot dir _) 'L' = r { dir = turnLeft dir }
    simulate' r@(Robot dir _) 'R' = r { dir = turnRight dir }

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft dir = pred dir

turnRight :: Bearing -> Bearing
turnRight West = North
turnRight dir = succ dir
