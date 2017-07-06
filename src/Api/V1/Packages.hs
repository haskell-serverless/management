{-# LANGUAGE OverloadedStrings #-}

module Api.V1.Packages
  (packages
  ) where

import Api (RoutesMap, methodNotAllowedResponse)
import Services.Packages (ListPackages, ReadPackage)
import Development.Placeholders
import Data.Maybe (fromJust)
import Data.Aeson (encode)
import Data.Binary.Builder (fromByteString)
import System.IO (withBinaryFile, IOMode(WriteMode))
import qualified System.IO.Streams as S (read)
import qualified Codec.Binary.UTF8.String as UTF8 (decode, encode)
import Control.Monad (unless)
import Network.Wai (Application, requestMethod, responseLBS, requestBody, responseStream, ResponseReceived)
import Network.HTTP.Types.Method (methodGet, methodPost, methodPut)
import Network.HTTP.Types.Header (hContentType, hContentLength)
import Network.HTTP.Types.Status (ok200, noContent204, methodNotAllowed405)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.Wai.Route

packages :: ListPackages -> ReadPackage ResponseReceived -> RoutesMap IO
packages serviceListPackages serviceReadPackage=
  [ ("/", listPackagesHandler serviceListPackages)
  , ("/:packageName", packagesHandler serviceReadPackage)
  ]

listPackagesHandler :: ListPackages -> Handler IO
listPackagesHandler serviceListPackages p rq respond = case requestMethod rq of
  method | method == methodGet -> getPackages serviceListPackages p rq respond
  _ -> respond methodNotAllowedResponse

getPackages :: ListPackages -> Handler IO
getPackages serviceListPackage _ rq respond =
  let headers = [(hContentType, "application/json")]
  in do
    packageListAsJson <- fmap encode serviceListPackage
    respond $ responseLBS ok200 headers packageListAsJson

packagesHandler :: ReadPackage ResponseReceived -> Handler IO
packagesHandler serviceReadPackage p rq respond = case requestMethod rq of
  method | method == methodGet -> getPackage serviceReadPackage p rq respond
  method | method == methodPut -> putPackage p rq respond
  _ -> respond methodNotAllowedResponse

packageName :: [(B.ByteString, B.ByteString)] -> String
packageName params = UTF8.decode $ B.unpack $ fromJust $ lookup "packageName" params

getPackage :: ReadPackage ResponseReceived -> Handler IO
getPackage serviceReadPackage params rq respond =
  serviceReadPackage (packageName params) createResponse
  where
    createResponse (fileSize, inputStream) =
      let fileSizeAsByteString = B.pack $ UTF8.encode $ show fileSize
          headers = [(hContentType, "application/octet-stream"), (hContentLength, fileSizeAsByteString)]
      in respond $ responseStream ok200 headers go
      where
        go write flush = do
          result <- S.read inputStream
          case result of
            Just someData -> do
              write $ fromByteString someData
              go write flush
            Nothing -> flush

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
