module Paths_ProjectEuler (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/washburn/.cabal/bin"
libdir     = "/home/washburn/.cabal/lib/ProjectEuler-0.1.0.0/ghc-7.8.4"
datadir    = "/home/washburn/.cabal/share/ProjectEuler-0.1.0.0"
libexecdir = "/home/washburn/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "ProjectEuler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ProjectEuler_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ProjectEuler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ProjectEuler_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
