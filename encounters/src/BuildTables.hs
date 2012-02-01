 {-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Maybe
import           Data.List
import           Data.ByteString.Internal
import qualified Data.Text.Encoding as T
import qualified Data.Text as DT
import qualified Text.XmlHtml as X
import           Snap.Extension.Heist
import           Snap.Extension.Timer
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist
import           Debug.Trace
import           CreatureTables
import           QueryLite
import           System.Environment

import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Database.HDBC.ColTypes as HDBCCT

buildTableIf :: [String] -> TableDef -> IO()
buildTableIf args t = 
    if ((toStr (getTableName t)) `elem` args) || ("all" `elem` args) then (buildTable t "test") else return ()

main :: IO()
main = do
    args <- getArgs
    if [] == args 
       then putStrLn "Usage: BuildTables table_name1 [table_name2 ...]"
       else sequence_ $  map (buildTableIf args) allCreatureTables


