{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
    ( api
    ) where

import Development.Placeholders
import Data.Maybe (fromJust)
import System.IO (withFile, IOMode(WriteMode))
import Codec.Binary.UTF8.String (decode)
import Control.Monad (unless)
import Network.Wai (Application, requestMethod, responseLBS, requestBody)
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

getPackage :: Handler m
getPackage p rq respond = respond $ responseLBS ok200 [] (LC.pack (show p))

putPackage :: Handler IO
putPackage params rq respond = do
    let packageName = fromJust $ lookup "packageName" params
    saveFile packageName
    respond $ responseLBS noContent204 [] LC.empty
  where
    saveFile packageName = withFile (decode $ B.unpack packageName) WriteMode go
    go handler = do
      bs <- requestBody rq
      unless (B.null bs) $ do
        B.hPut handler bs
        go handler

prependRoute :: B.ByteString -> RoutesMap m -> RoutesMap m
prependRoute prefix = map (\i -> ( B.concat [prefix, fst i], snd i))
