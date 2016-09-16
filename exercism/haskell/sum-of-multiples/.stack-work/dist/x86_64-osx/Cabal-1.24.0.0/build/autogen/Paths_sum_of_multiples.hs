{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_sum_of_multiples (
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

bindir     = "/Users/daewon/code/til/exercism/haskell/sum-of-multiples/.stack-work/install/x86_64-osx/nightly-2016-07-17/8.0.1/bin"
libdir     = "/Users/daewon/code/til/exercism/haskell/sum-of-multiples/.stack-work/install/x86_64-osx/nightly-2016-07-17/8.0.1/lib/x86_64-osx-ghc-8.0.1/sum-of-multiples-0.0.0-EuoT2hUAWAUULQUVU0l2y"
datadir    = "/Users/daewon/code/til/exercism/haskell/sum-of-multiples/.stack-work/install/x86_64-osx/nightly-2016-07-17/8.0.1/share/x86_64-osx-ghc-8.0.1/sum-of-multiples-0.0.0"
libexecdir = "/Users/daewon/code/til/exercism/haskell/sum-of-multiples/.stack-work/install/x86_64-osx/nightly-2016-07-17/8.0.1/libexec"
sysconfdir = "/Users/daewon/code/til/exercism/haskell/sum-of-multiples/.stack-work/install/x86_64-osx/nightly-2016-07-17/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sum_of_multiples_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sum_of_multiples_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "sum_of_multiples_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sum_of_multiples_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sum_of_multiples_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
