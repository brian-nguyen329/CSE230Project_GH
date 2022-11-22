{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_guitarhero (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/mnt/d/Users/bnguy/Desktop/FALL 2022/CSE 230/CSE230Project_GH/.stack-work/install/x86_64-linux-tinfo6/b93465ab0d282afe165880b1de029f47d8760dcc1c3bbb29261e6fc93640c009/8.10.4/bin"
libdir     = "/mnt/d/Users/bnguy/Desktop/FALL 2022/CSE 230/CSE230Project_GH/.stack-work/install/x86_64-linux-tinfo6/b93465ab0d282afe165880b1de029f47d8760dcc1c3bbb29261e6fc93640c009/8.10.4/lib/x86_64-linux-ghc-8.10.4/guitarhero-0.1.0.0-4Zubag7MBoNMbVvMxzWRQ-guitarhero"
dynlibdir  = "/mnt/d/Users/bnguy/Desktop/FALL 2022/CSE 230/CSE230Project_GH/.stack-work/install/x86_64-linux-tinfo6/b93465ab0d282afe165880b1de029f47d8760dcc1c3bbb29261e6fc93640c009/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/mnt/d/Users/bnguy/Desktop/FALL 2022/CSE 230/CSE230Project_GH/.stack-work/install/x86_64-linux-tinfo6/b93465ab0d282afe165880b1de029f47d8760dcc1c3bbb29261e6fc93640c009/8.10.4/share/x86_64-linux-ghc-8.10.4/guitarhero-0.1.0.0"
libexecdir = "/mnt/d/Users/bnguy/Desktop/FALL 2022/CSE 230/CSE230Project_GH/.stack-work/install/x86_64-linux-tinfo6/b93465ab0d282afe165880b1de029f47d8760dcc1c3bbb29261e6fc93640c009/8.10.4/libexec/x86_64-linux-ghc-8.10.4/guitarhero-0.1.0.0"
sysconfdir = "/mnt/d/Users/bnguy/Desktop/FALL 2022/CSE 230/CSE230Project_GH/.stack-work/install/x86_64-linux-tinfo6/b93465ab0d282afe165880b1de029f47d8760dcc1c3bbb29261e6fc93640c009/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "guitarhero_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "guitarhero_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "guitarhero_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "guitarhero_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "guitarhero_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "guitarhero_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
