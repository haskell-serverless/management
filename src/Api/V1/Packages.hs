{-# LANGUAGE OverloadedStrings #-}

module Api.V1.Packages
  (packages
  ) where

import Api (RoutesMap)
import Development.Placeholders
import Data.Maybe (fromJust)
import Data.Aeson (encode)
import System.IO (withBinaryFile, IOMode(WriteMode))
import Codec.Binary.UTF8.String (decode)
import Control.Monad (unless)
import Network.Wai (Application, requestMethod, responseLBS, requestBody, responseFile)
import Network.HTTP.Types.Method (methodGet, methodPost, methodPut)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (ok200, noContent204, methodNotAllowed405)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.Wai.Route

packages :: IO [String] -> RoutesMap IO
packages serviceListPackages =
  [ ("/", listPackagesHandler serviceListPackages)
  , ("/:packageName", packagesHandler)
  ]

listPackagesHandler :: IO [String] -> Handler IO
listPackagesHandler serviceListPackages p rq respond = case requestMethod rq of
  method | method == methodGet -> listPackages serviceListPackages p rq respond
  _ -> respond $ responseLBS methodNotAllowed405 [] LC.empty

listPackages :: IO [String] -> Handler IO
listPackages serviceListPackage _ rq respond = do
  onlyFileNames <- serviceListPackage
  respond $ responseLBS ok200 [(hContentType, "application/json")] (encode onlyFileNames)

packagesHandler :: Handler IO
packagesHandler p rq respond = case requestMethod rq of
  method | method == methodGet -> getPackage p rq respond
  method | method == methodPut -> putPackage p rq respond
  _ -> respond $ responseLBS methodNotAllowed405 [] LC.empty

packageName :: [(B.ByteString, B.ByteString)] -> String
packageName params = decode $ B.unpack $ fromJust $ lookup "packageName" params

getPackage :: Handler m
getPackage params rq respond =
  respond $ responseFile ok200 [(hContentType, "application/octet-stream")] (packageName params) Nothing

putPackage :: Handler IO
putPackage params rq respond = do
    saveFile $ packageName params
    respond $ responseLBS noContent204 [] LC.empty
  where
    saveFile fileName = withBinaryFile fileName WriteMode go
    go handler = do
      bs <- requestBody rq
      unless (B.null bs) $ do
        B.hPut handler bs
        go handler
