module Services.Packages
  ( listPackages
  , ListPackages
  , readPackage
  , ReadPackage
  ) where

import Control.Monad (filterM)
import Data.ByteString (ByteString)
import System.Directory (listDirectory, getCurrentDirectory, doesFileExist)
import System.FilePath (takeFileName)
import System.IO (withBinaryFile, hFileSize, Handle, IOMode(ReadMode))
import System.IO.Streams.Handle (handleToInputStream)
import System.IO.Streams (InputStream)

type ListPackages = IO [String]
type ReadPackage a = String -> ((Integer, InputStream ByteString) -> IO a) -> IO a

listPackages :: IO [String]
listPackages = fmap (map takeFileName) listOfFiles
  where
    listOfFiles = getCurrentDirectory >>= listDirectory >>= filterM doesFileExist

readPackage :: String -> ((Integer, InputStream ByteString) -> IO a) -> IO a
readPackage packageName thing =
  withBinaryFile packageName ReadMode doThing
  where
    doThing handle = do
      fileSize <- hFileSize handle
      inputStream <- handleToInputStream handle
      thing (fileSize, inputStream)
