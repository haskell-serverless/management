{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( api
    ) where

import Development.Placeholders
import Network.Wai (Application, requestMethod, responseLBS)
import Network.HTTP.Types.Method (methodGet, methodPost, methodPut)
import Network.HTTP.Types.Status (ok200)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.Wai.Route

type RoutesMap m = [(B.ByteString, Handler m)]

api :: Application
api = route $ prependRoute "/v1" v1

v1 :: RoutesMap m
v1 = prependRoute "/packages" packages

packages :: RoutesMap m
packages =
  [ ("/:packageName", packagesHandler)
  ]

packagesHandler :: Handler m
packagesHandler p rq = case requestMethod rq of
  method | method == methodGet -> getPackage p rq
  method | method == methodPut -> putPackage p rq
  _ -> $notImplemented

getPackage :: Handler m
getPackage p rq respond = respond $ responseLBS ok200 [] (LC.pack (show p))

putPackage :: Handler m
putPackage = $notImplemented

prependRoute :: B.ByteString -> RoutesMap m -> RoutesMap m
prependRoute prefix = map (\i -> ( B.concat [prefix, fst i], snd i))
