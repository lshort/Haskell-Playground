module Paths_encounters (
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
libdir     = "/home/los/.cabal/lib/encounters-0.1/ghc-7.0.2"
datadir    = "/home/los/.cabal/share/encounters-0.1"
libexecdir = "/home/los/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "encounters_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "encounters_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "encounters_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "encounters_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
