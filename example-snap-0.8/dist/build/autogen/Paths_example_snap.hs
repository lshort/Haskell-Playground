module Paths_example_snap (
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
libdir     = "/home/los/.cabal/lib/example-snap-0.1/ghc-7.0.2"
datadir    = "/home/los/.cabal/share/example-snap-0.1"
libexecdir = "/home/los/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "example_snap_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "example_snap_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "example_snap_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "example_snap_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
