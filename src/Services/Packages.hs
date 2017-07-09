module Services.Packages
  ( listPackages
  , ListPackages
  , readPackage
  , ReadPackage
  , savePackage
  , SavePackage
  ) where

import Control.Monad (filterM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import System.Directory (listDirectory, getCurrentDirectory, doesFileExist)
import System.FilePath (takeFileName)
import System.IO (withBinaryFile, hFileSize, Handle, IOMode(ReadMode, WriteMode))
import System.IO.Streams.Handle (handleToInputStream, handleToOutputStream)
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as ST

type ListPackages = IO [String]
type ReadPackage a = String -> ((Integer, InputStream ByteString) -> IO a) -> IO a
type SavePackage =  String -> InputStream ByteString -> IO ()

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

savePackage :: String -> InputStream ByteString -> IO ()
savePackage packageName inStream  =
  withBinaryFile packageName WriteMode saveOutputStream
  where
    saveOutputStream handle = do
      maybeChunk <- ST.read inStream
      case maybeChunk of
        Nothing -> return ()
        Just chunk -> do
          S.hPut handle chunk
          saveOutputStream handle
