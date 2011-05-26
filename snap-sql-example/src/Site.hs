{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

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


import           Application
import           Database.HDBC
import           Database.HDBC.PostgreSQL

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Application ()
index = ifTop $ heistLocal (bindSplices indexSplices) $ render "index"
  where
    indexSplices =
        [ ("start-time",   startTimeSplice)
        , ("current-time", currentTimeSplice)
        ]

------------------------------------------------------------------------------
-- | Renders the echo page.
echo :: Application ()
echo = do
    message <- decodedParam "stuff"
    heistLocal (bindString "message" (T.decodeUtf8 message)) $ render "echo"
  where
    decodedParam p = fromMaybe "" <$> getParam p

------------------------------------------------------------------------------
-- | Renders the cheese_template page.
my_template ::  Application ()
my_template= heistLocal templateSt $ render "cheese_template"
  where
    templateSt ts = bindSplices sqlSplices ts
    sqlSplices =
        [ ("get_sql_basic",   grabSQLbasic)
        , ("get_sql_where_attr",   grabSQLwhereAttr)
        , ("get_sql_where",   grabSQLwhere)
        ]


------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site  = route [ ("/",  index)
             , ("/cheese_template", my_template)
             , ("/echo/:stuff", echo)
             ]
       <|> serveDirectory "resources/static"


----------------------------------------------------------------------
-- | Uses tag attributes to run a query where key = value
grabSQLwhereAttr :: Splice Application
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
grabSQLwhere :: Splice Application
grabSQLwhere = do
    contents <- getParamNode
    let column = getChildText contents "column"
        clause = getChildText contents "where_clause"
        table  = getChildText contents "table"
    qval <- liftIO $ runQuery $ bldQueryWhereNew column table clause
    return $ [X.TextNode $ DT.pack $ qval]

-------------------------------------------------------------------------
-- | Uses child element contents to run a basic query with no clauses
grabSQLbasic :: Splice Application
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
runQuery x | trace ("runQuery " ++ show x) False = undefined
runQuery x = do
        conn <- connectPostgreSQL "dbname=test"
        results <- quickQuery' conn x []
        disconnect conn
        return $ (concat . intersperse "," . map fromSql . concat) results

