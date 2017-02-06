{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_RefSerialize (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,3,1,4] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.10.2\\RefSerialize-0.3.1.4-gsqojE0gNjFc2gKqBc88i"
datadir    = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.10.2\\RefSerialize-0.3.1.4"
libexecdir = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\RefSerialize-0.3.1.4-gsqojE0gNjFc2gKqBc88i"
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
