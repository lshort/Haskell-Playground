{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

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
import           Data.Char
import           Data.List
import           Data.ByteString.Internal
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text as DT
import qualified Text.XmlHtml as X
import           Snap.Extension.Heist
import           Snap.Extension.Timer
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist


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
        [ ("sql_text_input",   sqlTextInput)
        , ("uri_text_input",   uriTextInput)
        , ("uri_text_w_save",  uriTextWithSave)
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
-- | these are the database definitions
data TableSQL = TableSQL { databaseName :: String, tableName :: String, columns :: [String]  }

cheeseTable = TableSQL { databaseName = "test", tableName = "cheese", 
                         columns = ["name","country","price","stock"] }


----------------------------------------------------------------------
-- | Uses tag attributes to run an SQL query where key = value
--   and uses this data as the initial value in an input element
sqlTextInput :: Splice Application
sqlTextInput = do
    node <- getParamNode
    req <- lift getRequest
    let column = decode node req "column"
    let key    = decode node req "key"
    let value  = decode node req "value"
    let table  = decode node req "table"
    let label  = decode node req "label"
    ival <- liftIO $ runQuery $ bldQueryWhereEq column table key $ addQuote value
    let element = X.Element { X.elementTag = "input",
                              X.elementAttrs = [("type","text"),("value",DT.pack ival),("name",DT.pack column)], 
                              X.elementChildren = [] }
    return $ (X.TextNode $ DT.pack $ label ++ ": ") : [element]


----------------------------------------------------------------------
-- | Pulls node attributes from the splice and from the URI query data
--   and uses this data as the name, label, and initial value for an input element
uriTextInput :: Splice Application
uriTextInput = do
    node <- getParamNode
    req <- lift getRequest
    let val = decode node req "value"
    let label = decode node req "label"
    let name = decode node req "name"
    let element = X.Element { X.elementTag = "input",
                              X.elementAttrs = [("type","text"),("value",DT.pack val),("name",DT.pack name)], 
                              X.elementChildren = [] }
    return $ (X.TextNode $ DT.pack $ label ++ ": ") : [element]


----------------------------------------------------------------------
-- | just like uriTextInput, but first saves the current form data to SQL
uriTextWithSave :: Splice Application
uriTextWithSave = do
    saveDataIf cheeseTable
    uriTextInput

----------------------------------------------------------------------
-- | saves the current form data to SQL
--   must live in the Splice monad to get info from the splice and the URI query
saveDataIf :: TableSQL -> Splice Application
saveDataIf t = do
    req <- lift getRequest
    nada <- liftIO $ maybeSaveData req t
    return []

----------------------------------------------------------------------
-- | save the table data to SQL but only if ?name is set in the URI query data
--   and the ?_task is set to "save"
maybeSaveData :: Request -> TableSQL -> IO ()
maybeSaveData req t = do
    let task = getReqParam req "_task"
        name = getReqParam req "name"
    if (not $ null name) && (task == "save")
    then (saveTable req t)
    else return ()

----------------------------------------------------------------------
-- | save the data to SQL -- this will be an insert if there 
--   is no entry for the name, an update otherwise
saveTable :: Request -> TableSQL -> IO ()
saveTable req t = do
    let  name = getReqParam req "name"
    exists <- existsEntry (tableName t) "name" name
    if exists
    then updateTable req t
    else insertTable req t


----------------------------------------------------------------------
-- | update the data to SQL
updateTable :: Request -> TableSQL -> IO ()
updateTable req t = do
    conn <- connectPostgreSQL $ "dbname=" ++ (databaseName t)
    let values = map (toSql . getReqParam req) (map strToByteStr $ columns t)
        where_clause = "name=" ++ (addQuote $ getReqParam req "name")
        update  = bldUpdate (tableName t) (columns t) where_clause
    stmt <- prepare conn update
    execute stmt $ values
    commit conn
    disconnect conn

----------------------------------------------------------------------
-- | insert the data to SQL
insertTable :: Request -> TableSQL -> IO ()
insertTable req t = do
    conn <- connectPostgreSQL $ "dbname=" ++ (databaseName t)
    stmt <- prepare conn $ "INSERT INTO " ++ (tableName t) ++ " VALUES (" ++ qs ++ ")"
    let values = map (toSql . getReqParam req) (map strToByteStr $ columns t)
    execute stmt values
    commit conn
    disconnect conn
  where
    qs = concat $ intersperse "," $ replicate (length $ columns t) "?"

----------------------------------------------------------------------
-- | gets the value of the named attribute from the URI query string
getReqParam :: Request -> ByteString -> String
getReqParam req attr = 
    byteStrToStr $ head $ fromMaybe [BS.empty] $ rqParam attr req

-------------------------------------------------------------------------
-- | if the prefix is "uri:" then it pulls a data value from the URI query data 
--   else it returns the value of the attribute from the splice node
decode :: X.Node -> Request -> String -> String
decode n r s = 
   let y = getAttr n $ DT.pack s
       (a,b) = splitAt 4 y
   in if (a == "uri:")
      then byteStrToStr $ head $  fromMaybe [BS.empty] $ rqParam (strToByteStr b) r
      else y


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
-- | builds an SQL query string for updating a record
bldUpdate :: String -> [String] -> String -> String
bldUpdate table columns where_clause = 
    "UPDATE " ++ table ++ " SET " ++ update_list ++ " WHERE " ++ where_clause
  where
    updateEntry c = c ++ "=?"
    update_list = if null columns
                  then ""
                  else foldl (\t c -> t ++ ", " ++ updateEntry c) (updateEntry $ head columns) 
                                                                  (tail columns)

-------------------------------------------------------------------------
-- | returns True if there is an entry in the table where key==value
existsEntry :: String -> String -> String -> IO Bool
existsEntry table key value =  do
    results <- runQuery $ bldQueryWhereEq "*" table key $ addQuote value
    if "" == results
    then return False
    else return True

-------------------------------------------------------------------------
-- | builds a query with a WHERE EQUALS clause
bldQueryWhereEq :: String -> String -> String -> String -> Maybe String
bldQueryWhereEq col tab key val = 
  bldQueryBasic col tab +++ Just " WHERE " +++ notMT key +++ Just " = " +++ notMT val

-------------------------------------------------------------------------
-- | builds a query with a WHERE clause
bldQueryWhere :: String -> String -> String -> Maybe String
bldQueryWhere col tab whr = 
  bldQueryBasic col tab +++ Just " WHERE " +++ notMT whr

-------------------------------------------------------------------------
-- | builds a query with no clauses
bldQueryBasic :: String -> String -> Maybe String
bldQueryBasic col table = 
  Just "SELECT " +++ notMT col +++ Just " FROM " +++ notMT table

(+++) :: Maybe String -> Maybe String -> Maybe String
(+++) = liftM2 (++)

notMT :: String -> Maybe String
notMT [] = Nothing
notMT x = Just x

-------------------------------------------------------------------------
-- | runs a query
runQuery :: Maybe String -> IO String
runQuery Nothing = return ""
runQuery (Just x) =  do
        conn <- connectPostgreSQL "dbname=test"
        results <- quickQuery' conn x []
        disconnect conn
        return $ (concat . intersperse "," . map fromSql . concat) results


-------------------------------------------------------------------------
-- | builds an SQL query string for updating a record
addQuote :: String -> String
addQuote x = "'" ++ x ++ "'"


-------------------------------------------------------------------------
-- | ByteString conversion functions
--
byteStrToStr :: ByteString -> String
byteStrToStr x = map (chr . fromIntegral) $ BS.unpack x

strToByteStr :: String -> ByteString
strToByteStr x = BS.pack $ map (fromIntegral . ord) x
