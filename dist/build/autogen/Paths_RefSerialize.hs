module Paths_RefSerialize (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,2,8,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Users\\agocorona\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\agocorona\\AppData\\Roaming\\cabal\\RefSerialize-0.2.8.1\\ghc-7.0.3"
datadir    = "C:\\Users\\agocorona\\AppData\\Roaming\\cabal\\RefSerialize-0.2.8.1"
libexecdir = "C:\\Users\\agocorona\\AppData\\Roaming\\cabal\\RefSerialize-0.2.8.1"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "RefSerialize_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "RefSerialize_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "RefSerialize_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "RefSerialize_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
