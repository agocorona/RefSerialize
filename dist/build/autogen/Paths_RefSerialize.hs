module Paths_RefSerialize (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,3,1,3], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\i386-windows-ghc-7.8.2\\RefSerialize-0.3.1.3"
datadir    = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\i386-windows-ghc-7.8.2\\RefSerialize-0.3.1.3"
libexecdir = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\RefSerialize-0.3.1.3"
sysconfdir = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "RefSerialize_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "RefSerialize_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "RefSerialize_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "RefSerialize_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "RefSerialize_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
