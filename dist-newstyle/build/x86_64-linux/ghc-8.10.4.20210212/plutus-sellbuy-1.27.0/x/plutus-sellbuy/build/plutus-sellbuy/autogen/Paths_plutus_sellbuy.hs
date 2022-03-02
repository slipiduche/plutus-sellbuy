{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_plutus_sellbuy (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [1,27,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ubuntu/.cabal/bin"
libdir     = "/home/ubuntu/.cabal/lib/x86_64-linux-ghc-8.10.4.20210212/plutus-sellbuy-1.27.0-inplace-plutus-sellbuy"
dynlibdir  = "/home/ubuntu/.cabal/lib/x86_64-linux-ghc-8.10.4.20210212"
datadir    = "/home/ubuntu/.cabal/share/x86_64-linux-ghc-8.10.4.20210212/plutus-sellbuy-1.27.0"
libexecdir = "/home/ubuntu/.cabal/libexec/x86_64-linux-ghc-8.10.4.20210212/plutus-sellbuy-1.27.0"
sysconfdir = "/home/ubuntu/.cabal/etc"

getBinDir     = catchIO (getEnv "plutus_sellbuy_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "plutus_sellbuy_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "plutus_sellbuy_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "plutus_sellbuy_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "plutus_sellbuy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "plutus_sellbuy_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
