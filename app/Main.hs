module Main where

import Api
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8080 api
