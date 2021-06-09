{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hs_huffman (
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

bindir     = "/home/notoboto/.cabal/bin"
libdir     = "/home/notoboto/.cabal/lib/x86_64-linux-ghc-8.6.5/hs-huffman-0.1.0.0-FOXHvAgOsp16avumovBoo7"
dynlibdir  = "/home/notoboto/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/notoboto/.cabal/share/x86_64-linux-ghc-8.6.5/hs-huffman-0.1.0.0"
libexecdir = "/home/notoboto/.cabal/libexec/x86_64-linux-ghc-8.6.5/hs-huffman-0.1.0.0"
sysconfdir = "/home/notoboto/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hs_huffman_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hs_huffman_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hs_huffman_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hs_huffman_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hs_huffman_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hs_huffman_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
