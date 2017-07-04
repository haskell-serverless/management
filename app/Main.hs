module Main where

import Api (api)
import Api.V1 (v1)
import Api.V1.Packages (packages)
import Services.Packages (listPackages)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8080 (api $ v1 $ packages listPackages)
