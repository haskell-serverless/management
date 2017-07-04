{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
    ( api
    ) where

import Development.Placeholders
import Data.Maybe (fromJust)
import System.IO (withBinaryFile, IOMode(WriteMode))
import Codec.Binary.UTF8.String (decode)
import Control.Monad (unless)
import Network.Wai (Application, requestMethod, responseLBS, requestBody, responseFile)
import Network.HTTP.Types.Method (methodGet, methodPost, methodPut)
import Network.HTTP.Types.Status (ok200, noContent204)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.Wai.Route

type RoutesMap m = [(B.ByteString, Handler m)]

api :: Application
api = route $ prependRoute "/v1" v1

v1 :: RoutesMap IO
v1 = prependRoute "/packages" packages

packages :: RoutesMap IO
packages =
  [ ("/:packageName", packagesHandler)
  ]

packagesHandler :: Handler IO
packagesHandler p rq = case requestMethod rq of
  method | method == methodGet -> getPackage p rq
  method | method == methodPut -> putPackage p rq
  _ -> $notImplemented

packageName :: [(B.ByteString, B.ByteString)] -> String
packageName params = decode $ B.unpack $ fromJust $ lookup "packageName" params

getPackage :: Handler m
getPackage params rq respond =
  respond $ responseFile ok200 [] (packageName params) Nothing

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

prependRoute :: B.ByteString -> RoutesMap m -> RoutesMap m
prependRoute prefix = map (\i -> ( B.concat [prefix, fst i], snd i))
