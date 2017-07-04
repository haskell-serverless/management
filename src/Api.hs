{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
    ( api
    , RoutesMap
    , prependRoute
    ) where

import Development.Placeholders
import Data.Maybe (fromJust)
import Data.Aeson (encode)
import System.IO (withBinaryFile, IOMode(WriteMode))
import System.Directory (listDirectory, getCurrentDirectory, doesFileExist)
import System.FilePath (takeFileName)
import Codec.Binary.UTF8.String (decode)
import Control.Monad (unless, filterM)
import Network.Wai (Application, requestMethod, responseLBS, requestBody, responseFile)
import Network.HTTP.Types.Method (methodGet, methodPost, methodPut)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (ok200, noContent204, methodNotAllowed405)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.Wai.Route

type RoutesMap m = [(B.ByteString, Handler m)]

api :: RoutesMap IO -> Application
api v1Routes = route $ prependRoute "/v1" v1Routes

prependRoute :: B.ByteString -> RoutesMap m -> RoutesMap m
prependRoute prefix = map (\i -> ( B.concat [prefix, fst i], snd i))
