module Services.Packages
  ( listPackages
  , ListPackages
  ) where

import Control.Monad (filterM)
import System.Directory (listDirectory, getCurrentDirectory, doesFileExist)
import System.FilePath (takeFileName)

type ListPackages = IO [String]

listPackages :: IO [String]
listPackages = fmap (map takeFileName) listOfFiles
  where
    listOfFiles = getCurrentDirectory >>= listDirectory >>= filterM doesFileExist
