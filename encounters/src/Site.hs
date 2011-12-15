{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

{-|
This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.
-}

module Site
  ( site
  ) where

import           Query
import           CreatureTables
import           Application
import           Safe
import           System.IO.Unsafe
import           Data.List.Split

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

import           Debug.Trace
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
-- | 
getSplices :: [TableDef] -> [(DT.Text,Splice Application)]
getSplices t = 
        [ ("sql_text_input",   sqlTextInput)
        , ("sql_check_box",    sqlCheckBox)
        , ("sql_radio",        sqlRadio)
        , ("sql_drop",         sqlDrop False)
        , ("sql_drop_w_none",  sqlDrop True)
        , ("sql_drop_w_save",  sqlDropSave t)
        , ("uri_text_input",   uriTextInput)
        , ("uri_hidden_input", uriHiddenInput)
        , ("uri_text_w_save",  uriTextWithSave t)
        ]


------------------------------------------------------------------------------
-- | Renders the creature_template page.
creature_template ::  Application ()
creature_template = heistLocal templateSt $ render "creatures"
  where
    templateSt ts = bindSplices (getSplices allCreatureTables) ts

template_template ::  Application ()
template_template = heistLocal templateSt $ render "templates"
  where
    templateSt ts = bindSplices (getSplices allCreatureTables) ts

level_template ::  Application ()
level_template = heistLocal templateSt $ render "levels"
  where
    templateSt ts = bindSplices (getSplices allCreatureTables) ts

region_template ::  Application ()
region_template = heistLocal templateSt $ render "region_and_encounter_page"
  where
    templateSt ts = bindSplices (getSplices allCreatureTables) ts


------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site  = route [ ("/",  index)
             , ("/creature_template", creature_template)
             , ("/template_template", template_template)
             , ("/level_template",    level_template)
             , ("/region_template",   region_template)
             , ("/echo/:stuff", echo)
             ]
       <|> serveDirectory "resources/static"



----------------------------------------------------------------------
-- | Uses tag attributes to run an SQL query where key = value
--   and uses this data as the initial value in an input element
sqlCheckBox :: Splice Application
sqlCheckBox = do
    node <- getParamNode
    req <- lift getRequest
    let column = ColumnName $ decode node req "column"
        key    = ColumnName $ decode node req "key"
        keyval = decode node req "keyval"
        table  = TableName $ decode node req "table"
        value  = decode node req "value"
        label  = decode node req "label"
    ival <- liftIO $ runQuery $ bldQueryWhereEq column table key $ addQuote keyval
    let checked =  [("checked","checked") | ival==value ] 
--    let checked = if ival == value then [("checked","checked")] else []
        element = X.Element { X.elementTag = "input",
                              X.elementAttrs = ("type","checkbox"):("value",DT.pack value):
                                               ("name",DT.pack $ toStr column):checked, 
                              X.elementChildren = [] }
    return $ (X.TextNode $ DT.pack $ label ++ ": ") : [element]


----------------------------------------------------------------------
-- | Uses tag attributes to run an SQL query over the whole table
--   and uses this data as the initial value in an input element
--   also saves the data
sqlDropSave :: [TableDef] -> Splice Application
sqlDropSave ts = do
    maybeSave ts
    sqlDropForm

sqlDropForm :: Splice Application
sqlDropForm  = do
    node <- getParamNode
    req <- lift getRequest
    let column      = ColumnName $ decode node req "column"
        value       = decode node req "value"
        action      = decode node req "action"
        keys        = decode node req "keys"
        table_text  = decode node req "table"
        table       = TableName table_text
        hiddenKids  = [bldHiddenInput req x | x <- childElements node]
    ival <- if keys == ""
        then liftIO $ runQueryList $ bldQueryBasic column table
        else liftIO $ getQueryVals node req
    let kinder = [buildOpt (DT.pack v) (v==value) | v <- sort ival]
        sel_name  = getKeyName' table column
        sel_elmt  = X.Element { X.elementTag = "select",
                               X.elementAttrs = [("name",sel_name)],
                               X.elementChildren = kinder }
        sub_elmt  = X.Element { X.elementTag = "input", 
                               X.elementAttrs = [("type","submit"),("value","Edit")],
                               X.elementChildren = [] }
        form_name = DT.pack $ (toUpper $ head table_text) : (tail table_text ++ " ID")
        children = sel_elmt:sub_elmt:hiddenKids 
        form_elmt = X.Element { X.elementTag = "form",
                               X.elementAttrs = [("method","get"),("name",form_name),
                                                 ("action", DT.pack action)],
                               X.elementChildren = children }
    return $ X.TextNode form_name : [form_elmt] 
  where
    childElements x = X.childElementsTag "uri_hidden_input" x


sqlDrop :: Bool -> Splice Application
sqlDrop none_opt = do
    node <- getParamNode
    req <- lift getRequest
    let column      = ColumnName $ decode node req "column"
        initval     = decode node req "initval"
        keys        = decode node req "keys"
        name        = decode node req "name"
        table_text  = decode node req "table"
        table       = TableName table_text
    ival <- if keys == ""
        then liftIO $ runQueryList $ bldQueryBasic column table
        else liftIO $ getQueryVals node req
    let ival' = if none_opt  then ival ++ [""]  else ival
        kinder = [buildOpt (DT.pack v) (v==initval) | v <- sort ival']
        sel_elmt  = X.Element { X.elementTag = "select",
                               X.elementAttrs = [("name", DT.pack name)],
                               X.elementChildren = kinder }
    return [sel_elmt]


buildOpt :: DT.Text -> Bool -> X.Node
--buildOpt text selected | trace ( (show text) ++ " " ++ (show selected) ) False = undefined
buildOpt text selected = 
--   let selection = if selected then [("selected","selected")] else []
   let selection = [("selected","selected") | selected]
   in  X.Element { X.elementTag = "option",
                   X.elementAttrs = ("value",text):selection,
                   X.elementChildren = [X.TextNode text] }


bldHiddenInput :: Request -> X.Node -> X.Node
bldHiddenInput req node = 
    let name  = decode node req "name"
        value = decode node req "value"
    in X.Element { X.elementTag = "input",
                   X.elementAttrs = [("type","hidden"),("value",DT.pack value),
                                     ("name",DT.pack name)], 
                   X.elementChildren = [] }

----------------------------------------------------------------------
-- | Uses tag attributes to run an SQL query where key = value
--   and uses this data as the initial value in an input element
sqlRadio :: Splice Application
sqlRadio = do
    node <- getParamNode
    req <- lift getRequest
    let column_text = decode node req "column"
        value  = decode node req "value"
        table  = TableName $ decode node req "table"
    ival <- liftIO $ liftM (headDef "") $ getQueryVals node req
    let checked = [("checked","checked") | ival==value ]
--    let checked = if ival == value then [("checked","")] else []
        ident = DT.pack $ column_text ++ "_" ++ value
        el_name  = getKeyName' table (ColumnName column_text)
        attrs = [("type","radio"),("value",DT.pack value),("name",el_name),("id",ident)] ++ checked
        label   = X.Element { X.elementTag = "label",
                              X.elementAttrs = [("for",ident)],
                              X.elementChildren = [X.TextNode $ DT.pack value] }
        element = X.Element { X.elementTag = "input",
                              X.elementAttrs = attrs,
                              X.elementChildren = [] }
        spacer  = X.TextNode $ DT.pack spaces
    return $ spacer : label : element : [spacer]
  where
    spaces = " &nbsp "

----------------------------------------------------------------------
-- | Uses tag attributes to run an SQL query where key = value
--   and uses this data as the initial value in an input element
sqlTextInput :: Splice Application
sqlTextInput = do
    node <- getParamNode
    req <- lift getRequest
    let column = ColumnName $ decode node req "column"
        table  = TableName $ decode node req "table"
        label  = decode node req "label"
        size   = decode node req "size"
        value  = decode node req "value"
    ival <- liftIO $ liftM (headDef "") $ getQueryVals node req
    let el_name  = getKeyName' table column
        val = if value /= "" then value else ival
        element = X.Element { X.elementTag = "input",
                              X.elementAttrs = [("type","text"),("value",DT.pack val),
                                                ("name",el_name),("size",DT.pack size)], 
                              X.elementChildren = [] }
        label_text = if label /= "" then label ++ ":" else ""
    return $ (X.TextNode $ DT.pack $ label_text) : [element]

getQueryVals :: X.Node -> Request -> IO [String]
getQueryVals node req = do 
    let column      = ColumnName $ decode node req "column"
        table       = TableName $ decode node req "table"
        keyCount    = decode node req "keys"
        key_count   = readDef 0 keyCount
        key         = decode node req "key"
        value       = decode node req "keyval"
        key_nums    = [1..key_count] :: [Integer]
        keys    = map (ColumnName . decode node req . ("key" ++) . show) key_nums
        values  = map (decode node req . ("keyval" ++) . show) key_nums
    if key /= "" 
       then  runQueryList $ bldQueryWhereEq column table (ColumnName key) $ addQuote value
       else  runQueryList $ bldQueryMultiEq column table keys values

----------------------------------------------------------------------
-- | Pulls node attributes from the splice and from the URI query data
--   and uses this data as the name, label, and initial value for an input element
uriHiddenInput :: Splice Application
uriHiddenInput = do
    node <- getParamNode
    req <- lift getRequest
    return [bldHiddenInput req node]


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
                              X.elementAttrs = [("type","text"),("value",DT.pack val),
                                                ("name",DT.pack name)], 
                              X.elementChildren = [] }
    return $ (X.TextNode $ DT.pack $ label ++ ": ") : [element]


----------------------------------------------------------------------
-- | just like uriTextInput, but first saves the current form data to SQL
uriTextWithSave :: [TableDef] -> Splice Application
uriTextWithSave ts = do
    maybeSave ts
    uriTextInput

----------------------------------------------------------------------
-- | saves the current form data to SQL
--   must live in the Splice monad to get info from the splice and the URI query
maybeSave :: [TableDef] -> Splice Application
maybeSave tables = do
    req <- lift getRequest
    node <- getParamNode
    let table_name = TableName $ decode node req "table"
        key = buildElName table_name (ColumnName $ decode node req "column")
        save_task  = decode node req "save_task" 
        save       = if save_task == "" then "save" else save_task
    ignore <- liftIO $ maybeSaveData req tables table_name key save
    return []

----------------------------------------------------------------------
-- | save the table data to SQL but only if ?key is set in the URI query data
--   and the ?_task is set to "save"
maybeSaveData :: Request -> [TableDef] -> TableName -> UriKeyName -> String -> IO ()
--maybeSaveData req ts n key save | trace ("maybeSaveData " ++ save) False = undefined
maybeSaveData req tables table_name key@(UriKeyName k) save  = do
    let task      = getReqParam req "_task"
        keyname   = getReqParam req $ strToByteStr k
    if not (null keyname) && task == save
     then (saveTable req tables table_name key)
     else return ()

----------------------------------------------------------------------
-- | save the data to SQL -- this will be an insert if there 
--   is no entry for the name, an update otherwise
saveTable :: Request -> [TableDef] -> TableName -> UriKeyName -> IO ()
--saveTable req ts n key | trace ("saveTable " ++ n) False = undefined
saveTable req tables table_name key@(UriKeyName k) = do
    let  keyval = getReqParam req $ strToByteStr k
         t = fromMaybe emptyTable $ findTable table_name tables
         del = getReqParam req "delete"
    exists <- existsKeyVal table_name (ColumnName k) keyval
    if del /= ""
        then when exists $ deleteFromTable req t key
        else if exists
              then updateTable req t key
              else insertTable req t


----------------------------------------------------------------------
-- | delete the entry in the sql table
deleteFromTable :: Request -> TableDef -> UriKeyName -> IO ()
--deleteFromTable req t key | trace "deleteFromTable" False = undefined
deleteFromTable req t key@(UriKeyName k) = do
    conn <- connectPostgreSQL "dbname=test"
    let quoted_keyval = addQuote $ getReqParam req $ strToByteStr k
        where_clause = WhereClause $ (toStr $ getColName key) ++ "=" ++ quoted_keyval
        update  = bldDelete (getTableName t) where_clause
    stmt <- prepare conn update
    execute stmt []
    commit conn
    disconnect conn

----------------------------------------------------------------------
-- | update the data to SQL
updateTable :: Request -> TableDef -> UriKeyName -> IO ()
--updateTable req t key | trace "updateTable" False = undefined
updateTable req t key@(UriKeyName k) = do
    conn <- connectPostgreSQL $ "dbname=test"
    let tableName = getTableName t
        values = map (toSql . getReqParam req . strToByteStr . toStr . buildElName tableName ) $ getColumns t
        quoted_keyval = addQuote $ getReqParam req $ strToByteStr k
        where_clause = WhereClause $ (toStr $ getColName key) ++ "=" ++ quoted_keyval
        update  = bldUpdate tableName (getColumns t) where_clause
    stmt <- prepare conn update
    execute stmt values
    commit conn
    disconnect conn

----------------------------------------------------------------------
-- | insert the data to SQL
insertTable :: Request -> TableDef -> IO ()
--insertTable req t | trace "insertTable" False = undefined
insertTable req t@(TableDef _ a _ _) = do
    conn <- connectPostgreSQL "dbname=test"
    let cols = getColumns t
        tableName = getTableName t
        form_vals = map (getReqParam req . strToByteStr . toStr . buildElName tableName) cols
        values = zipWith getSqlVal a form_vals
        qs = intercalate "," $ replicate (length cols) "?"
        query = "INSERT INTO " ++ (toStr $ getTableName t) ++ " VALUES (" ++ qs ++ ")"
    stmt <- prepare conn query
    execute stmt values
    commit conn
    disconnect conn

----------------------------------------------------------------------
-- | gets the value of the named attribute from the URI query string
getReqParam :: Request -> ByteString -> String
--getReqParam req string  | trace ("getReqParam " ++ show string  ) False = undefined
getReqParam req attr = 
    byteStrToStr $ head $ fromMaybe [BS.empty] $ rqParam attr req

-------------------------------------------------------------------------
-- | if the prefix is "uri:" then it pulls a data value from the URI query data 
--   else it returns the value of the attribute from the splice node
--   if the prefix is "sqN:" then it runs an sql query with up to N colum matches
--       DO NOT nest sqN: calls but uri: may be nested inside sqN:
decode :: X.Node -> Request -> String -> String
decode n r s = decodeStr n r $ getAttr n $ DT.pack s

-- | decodes a literal string
decodeStr :: X.Node -> Request -> String -> String
decodeStr n r s = 
   let (a,b) = splitAt 4 s
   in case a of "uri:" -> getReqParam r (strToByteStr b)
                "sql:" -> unsafePerformIO $ getSql b n r
                _ -> s

-------------------------------------------------------------------------
-- | pulls sql value out, matching N column names 
getSql :: String -> X.Node -> Request -> IO String
--getSql string node req | trace ("getSql " ++ string ++ " " ++ (show node) ++ " " ++ (show req)) False = undefined
--getSql string node req | trace ("getSql " ++ string ) False = undefined
getSql x n r = do
   let xs = splitWhen (==',') x
       table  = TableName $ xs !! 0
       column = ColumnName $ xs !! 1
       key_count = readDef 0 $ xs !! 2
       keys      = map ColumnName (take key_count $ drop 3 xs)
       values    = map (decodeStr n r) (drop (3 + key_count) xs)
   runQuery $ bldQueryMultiEq column table keys values

-------------------------------------------------------------------------
-- | gets the value of the specified attribute from the node
getAttr :: X.Node -> DT.Text -> String
getAttr x y = DT.unpack $ fromMaybe (DT.pack "") (X.getAttribute y x)

-------------------------------------------------------------------------
-- | gets the text content of the specified child of the node
getChildText :: X.Node -> DT.Text -> String
getChildText x y = DT.unpack $ fromMaybe (DT.pack "") (liftM X.nodeText tag)
    where tag = X.childElementTag y x 


-------------------------------------------------------------------------
-- | ByteString conversion functions
--
byteStrToStr :: ByteString -> String
byteStrToStr x = map (chr . fromIntegral) $ BS.unpack x

strToByteStr :: String -> ByteString
strToByteStr x = BS.pack $ map (fromIntegral . ord) x

