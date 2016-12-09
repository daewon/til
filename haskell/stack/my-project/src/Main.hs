{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Web.Scotty
import Data.Monoid ((<>))
import System.IO (readFile)
import Data.Time (getCurrentTime)
import Data.Aeson

data User = User
  {
    userId :: Int,
    userName :: String
  } deriving (Show, Eq)

instance ToJson User
instance FromJSON User

printConfig = do
  contents <- readFile "stack.yaml"
  putStrLn contents

route :: ScottyM ()
route = do
  get "/hello/:name" $ do
    name <- param "name"
    text ("hello " <> name <> "!")

bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 1, userName = "jenny" }

allUsers :: [User]
allUsers = [bob, jenny]

hello :: ActionM ()
hello = do
  text "hello world"

main :: IO ()
main = do
  putStrLn "hello world"
  scotty 3000 $ route
