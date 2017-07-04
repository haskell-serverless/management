{-# LANGUAGE OverloadedStrings #-}

module Api.V1
  (v1
  ) where

import Api (RoutesMap, prependRoute)

v1 :: RoutesMap IO -> RoutesMap IO
v1 packagesRoutes = prependRoute "/packages" packagesRoutes
