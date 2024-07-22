module Main where

import Network.Wai.Handler.Warp (run)
import CRUD.Core (app)

-- Main function to start the server
main :: IO ()
main = do
  putStrLn "Server started at port 8080"
  run 8080 app -- or run 8080 app1

-- Test function
test :: IO ()
test = putStrLn "Hello from Main"