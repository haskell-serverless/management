module Services.Packages
  ( listPackages
  , ListPackages
  ) where

import Control.Monad (filterM)
import System.Directory (listDirectory, getCurrentDirectory, doesFileExist)
import System.FilePath (takeFileName)

type ListPackages = IO [String]

listPackages :: IO [String]
listPackages = do
  currDir <- getCurrentDirectory
  allEntries <- listDirectory currDir
  onlyFiles <- filterM doesFileExist allEntries
  return $ map takeFileName onlyFiles
