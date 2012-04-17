{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.State
import qualified Control.Exception as EXC
import           Data.Time.Clock
import           Data.Maybe
import           Data.List
import           Data.ByteString.Internal
import qualified Data.Text.Encoding as T
import qualified Data.Text as DT
import qualified Text.XmlHtml as X
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist
import           Debug.Trace
import           Application
import           Database.HDBC
import           Database.HDBC.Sqlite3

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: AppHandler ()
index = ifTop $ heistLocal (bindSplices indexSplices) $ render "index"
  where
    indexSplices =
        [ ("current-time", currentTimeSplice)
        , ("start-time",   startTimeSplice)
        ]

------------------------------------------------------------------------------
-- | Renders the echo page.
echo :: AppHandler ()
echo = do
    message <- decodedParam "stuff"
    heistLocal (bindString "message" (T.decodeUtf8 message)) $ render "echo"
  where
    decodedParam p = fromMaybe "" <$> getParam p

------------------------------------------------------------------------------
-- | Renders the cheese_template page.
my_template ::  AppHandler ()
my_template= heistLocal templateSt $ render "cheese_template"
  where
    templateSt ts = bindSplices sqlSplices ts
    sqlSplices =
        [ ("get_sql_basic",   grabSQLbasic)
        , ("get_sql_where_attr",   grabSQLwhereAttr)
        , ("get_sql_where",   grabSQLwhere)
        ]


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/",            index)
         , ("/echo/:stuff", echo)
         , ("/cheese_template", my_template)
         , ("", with heist heistServe)
         , ("", serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    sTime <- liftIO getCurrentTime
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    addRoutes routes
    return $ App h sTime

----------------------------------------------------------------------
-- | Uses tag attributes to run a query where key = value
grabSQLwhereAttr :: Splice AppHandler
grabSQLwhereAttr = do
    contents <- getParamNode
    let column = getAttr contents "column"
        key    = getAttr contents "key"
        value  = getAttr contents "value"
        table  = getAttr contents "table"
    qval <- liftIO $ runQuery $ bldQueryWhere column table key value
    return $ [X.TextNode $ DT.pack $ qval]

-----------------------------------------------------------------------
-- | Uses child element contents to run a query with a general where clause
grabSQLwhere :: Splice AppHandler
grabSQLwhere = do
    contents <- getParamNode
    let column = getChildText contents "column"
        clause = getChildText contents "where_clause"
        table  = getChildText contents "table"
    qval <- liftIO $ runQuery $ bldQueryWhereNew column table clause
    return $ [X.TextNode $ DT.pack $ qval]

-------------------------------------------------------------------------
-- | Uses child element contents to run a basic query with no clauses
grabSQLbasic :: Splice AppHandler
grabSQLbasic = do
    contents <- getParamNode
    let column = getChildText contents "column"
        table  = getChildText contents "table"
    qval <- liftIO $ runQuery $ bldQueryBasic column table 
    return $ [X.TextNode $ DT.pack $ qval]


-------------------------------------------------------------------------
-- | gets the value of the specified attribute from the node
getAttr :: X.Node -> DT.Text -> String
getAttr x y = DT.unpack $ maybe (DT.pack "") id (X.getAttribute y x)

-------------------------------------------------------------------------
-- | gets the text content of the specified child of the node
getChildText :: X.Node -> DT.Text -> String
getChildText x y = DT.unpack $ maybe (DT.pack "") id (liftM X.nodeText tag)
    where tag = X.childElementTag y x 

-------------------------------------------------------------------------
-- | builds a query with a WHERE EQUALS clause
bldQueryWhere :: String -> String -> String -> String -> String
bldQueryWhere col tab key val = bldQueryBasic col tab ++ " WHERE " 
                                ++ key ++ " = " ++ val

-------------------------------------------------------------------------
-- | builds a query with a WHERE clause
bldQueryWhereNew :: String -> String -> String -> String
bldQueryWhereNew col tab whr = bldQueryBasic col tab ++ " WHERE " ++ whr

-------------------------------------------------------------------------
-- | builds a query with no clauses
bldQueryBasic :: String -> String -> String
bldQueryBasic col tab = "SELECT " ++ col ++ " FROM " ++ tab

-------------------------------------------------------------------------
-- | runs a query
runQuery :: String -> IO String
runQuery x = do
        conn <- connectSqlite3 "test_db"
        results <- EXC.catch (quickQuery' conn x []) catchQueryExc
        disconnect conn
        return $ (concat . intersperse "," . map fromSql . concat) results

-----
-- | catch a query exception and return the empty list (of results)
--
catchQueryExc :: SqlError -> IO [[SqlValue]]
catchQueryExc x =  do
     putStrLn $ "SQL Error, query is: " ++ show x
     return []

------------------------------------------------------------------------------
-- | For your convenience, a splice which shows the start time.
startTimeSplice :: Splice AppHandler
startTimeSplice = do
    time <- lift $ gets _startTime
    return $ [X.TextNode $ DT.pack $ show $ time]


------------------------------------------------------------------------------
-- | For your convenience, a splice which shows the current time.
currentTimeSplice :: Splice AppHandler
currentTimeSplice = do
    time <- liftIO getCurrentTime
    return $ [X.TextNode $ DT.pack $ show $ time]
