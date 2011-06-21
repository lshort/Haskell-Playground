module Paths_cheese (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/los/.cabal/bin"
libdir     = "/home/los/.cabal/lib/cheese-0.1/ghc-7.0.2"
datadir    = "/home/los/.cabal/share/cheese-0.1"
libexecdir = "/home/los/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "cheese_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "cheese_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "cheese_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "cheese_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
