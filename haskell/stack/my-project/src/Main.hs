module Main where

import System.IO (readFile)
import Data.Time (getCurrentTime)

printConfig = do
  contents <- readFile "stack.yaml"
  putStrLn contents

main :: IO ()
main = do
  time <- getCurrentTime
  putStrLn $ show time
