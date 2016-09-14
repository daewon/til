{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_sublist (
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
version = Version [0,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/daewon/code/til/exercism/haskell/sublist/.stack-work/install/x86_64-osx/nightly-2016-07-17/8.0.1/bin"
libdir     = "/Users/daewon/code/til/exercism/haskell/sublist/.stack-work/install/x86_64-osx/nightly-2016-07-17/8.0.1/lib/x86_64-osx-ghc-8.0.1/sublist-0.0.0-LzccPXKc2D2oqD0LpMUVh"
datadir    = "/Users/daewon/code/til/exercism/haskell/sublist/.stack-work/install/x86_64-osx/nightly-2016-07-17/8.0.1/share/x86_64-osx-ghc-8.0.1/sublist-0.0.0"
libexecdir = "/Users/daewon/code/til/exercism/haskell/sublist/.stack-work/install/x86_64-osx/nightly-2016-07-17/8.0.1/libexec"
sysconfdir = "/Users/daewon/code/til/exercism/haskell/sublist/.stack-work/install/x86_64-osx/nightly-2016-07-17/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sublist_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sublist_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "sublist_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sublist_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sublist_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
