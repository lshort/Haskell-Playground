module Paths_snap_sql_example (
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
libdir     = "/home/los/.cabal/lib/snap-sql-example-0.1/ghc-7.0.2"
datadir    = "/home/los/.cabal/share/snap-sql-example-0.1"
libexecdir = "/home/los/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "snap_sql_example_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "snap_sql_example_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "snap_sql_example_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "snap_sql_example_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)