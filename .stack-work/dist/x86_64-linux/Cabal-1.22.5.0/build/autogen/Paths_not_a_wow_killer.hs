module Paths_not_a_wow_killer (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/data-big/dev/not-a-wow-killer/.stack-work/install/x86_64-linux/lts-6.16/7.10.3/bin"
libdir     = "/data-big/dev/not-a-wow-killer/.stack-work/install/x86_64-linux/lts-6.16/7.10.3/lib/x86_64-linux-ghc-7.10.3/not-a-wow-killer-0.1.0.0-ITG68z8a2ugG2Ngs416P0e"
datadir    = "/data-big/dev/not-a-wow-killer/.stack-work/install/x86_64-linux/lts-6.16/7.10.3/share/x86_64-linux-ghc-7.10.3/not-a-wow-killer-0.1.0.0"
libexecdir = "/data-big/dev/not-a-wow-killer/.stack-work/install/x86_64-linux/lts-6.16/7.10.3/libexec"
sysconfdir = "/data-big/dev/not-a-wow-killer/.stack-work/install/x86_64-linux/lts-6.16/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "not_a_wow_killer_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "not_a_wow_killer_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "not_a_wow_killer_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "not_a_wow_killer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "not_a_wow_killer_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
