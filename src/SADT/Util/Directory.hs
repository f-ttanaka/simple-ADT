module SADT.Util.Directory where

import SADT.Common
import System.Directory
import System.FilePath

-- get all files in the directory
getAllFilePaths :: FilePath -> IO [FilePath]
getAllFilePaths dir = do
  fs <- listDirectory dir
  concat <$> mapM makeFullPath fs
  where
    makeFullPath :: FilePath -> IO [FilePath]
    makeFullPath f = do
      let path = dir </> f
      isDir <- liftIO $ doesDirectoryExist path
      if isDir
        then getAllFilePaths path
        else return [path]
