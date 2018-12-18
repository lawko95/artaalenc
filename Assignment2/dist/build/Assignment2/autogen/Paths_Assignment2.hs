{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Assignment2 (
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

bindir     = "C:\\Users\\Berk\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Berk\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1\\Assignment2-0.1.0.0-9o5lCa5j7U862IPN0E9LcF"
dynlibdir  = "C:\\Users\\Berk\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1"
datadir    = "C:\\Users\\Berk\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1\\Assignment2-0.1.0.0"
libexecdir = "C:\\Users\\Berk\\AppData\\Roaming\\cabal\\Assignment2-0.1.0.0-9o5lCa5j7U862IPN0E9LcF\\x86_64-windows-ghc-8.2.1\\Assignment2-0.1.0.0"
sysconfdir = "C:\\Users\\Berk\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Assignment2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Assignment2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Assignment2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Assignment2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Assignment2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Assignment2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
