{-# LANGUAGE DoAndIfThenElse #-}

module QueryLite (
  PrimaryKeys, ForeignKey, SqlColumn(..), TableDef(..), TableName(..), ColumnName(..),
  UriKeyName(..), WhereClause(..), 
  bldQueryWhereEq, bldQueryWhere, bldQueryBasic, runQuery, runQueryList, getTypeName, 
  buildTable, findTable, getColumns, getTableName, getColumnName, emptyTable, 
  bldUpdate, bldDelete, bldQueryMultiEq, addQuote, existsKeyVal, existsByClause, getSqlVal, 
  getKeyName, getKeyName', getColName, buildElName, IsStr, toStr, makeColumn, runQueryNative,
  bldQueryEq, getFieldEq
)  where 

import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Database.HDBC.ColTypes as HDBCCT
import           Database.HDBC.SqlValue
import           Control.Applicative
import           Control.Exception as EXC
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
import           Debug.Trace

type PrimaryKeys = [String] 
type ForeignKey = (String,String)
type DatabaseName = String
type TypeName = String
data SqlColumn = SqlColumn ColumnName HDBCCT.SqlTypeId (Maybe Int) (Maybe Int) deriving (Show)
data TableDef = TableDef TableName [SqlColumn] PrimaryKeys [ForeignKey] deriving (Show)

makeColumn :: String -> HDBCCT.SqlTypeId -> (Maybe Int) -> (Maybe Int) -> SqlColumn
makeColumn col typ x y = SqlColumn (ColumnName col) typ x y

emptyTable :: TableDef
emptyTable = TableDef (TableName "") [] [] []

getTypeName :: HDBCCT.SqlTypeId -> String
getTypeName SqlVarCharT  = "varchar"
getTypeName SqlIntegerT  = "int"
getTypeName SqlDecimalT  = "decimal"
getTypeName SqlRealT     = "real"
getTypeName SqlBitT      = "boolean"
getTypeName _            = undefined

getDefault :: HDBCCT.SqlTypeId -> String
getDefault SqlVarCharT  = ""
getDefault SqlIntegerT  = "0"
getDefault SqlDecimalT  = "0.0"
getDefault SqlRealT     = "0.0"
getDefault SqlBitT      = "false"
getDefault _            = undefined


buildCore :: HDBCCT.SqlTypeId -> String -> String
buildCore t n = n ++ " " ++ getTypeName t

buildField :: SqlColumn -> String
buildField (SqlColumn (ColumnName n) t Nothing Nothing) = buildCore t n
buildField (SqlColumn (ColumnName n) t (Just x) Nothing) = buildCore t n ++ "(" ++ show x ++ ")"
buildField (SqlColumn (ColumnName n) t (Just x) (Just y)) = 
    buildCore t n ++ "(" ++ show x ++ "," ++ show y ++ ")"

buildPriKey :: PrimaryKeys -> String
buildPriKey [] = ""
buildPriKey xs = ", PRIMARY KEY (" ++ (intercalate "," xs) ++ ")"

buildForKey :: ForeignKey -> String
buildForKey (a,b) = ", FOREIGN KEY (" ++ a ++ ") references " ++ b  

buildFieldsStr :: TableDef -> String
buildFieldsStr (TableDef _ a b c) = "(" ++ buildFieldsStr' a ++ buildPriKey b
                                      ++ concatMap buildForKey c ++ ")"
  where buildFieldsStr' [] = ""
        buildFieldsStr' [x] = buildField x
        buildFieldsStr' (x:xs) = buildField x ++ ", " ++ buildFieldsStr' xs

buildTable :: TableDef -> String -> IO ()
buildTable col@(TableDef (TableName t) _ _ _) d = do
    conn <- connectSqlite3 $ "dbname=" ++  d
    let drop_q = "DROP TABLE IF EXISTS " ++ t 
        create_stmt = "CREATE TABLE " ++ t ++ buildFieldsStr col
    stmt1 <- prepare conn drop_q
    execute stmt1 []
    stmt2 <- prepare conn create_stmt
    putStrLn ("***** Building " ++ t)
    execute stmt2 []
    commit conn
    disconnect conn

findTable :: TableName -> [TableDef] -> Maybe TableDef
findTable _ [] = Nothing
findTable t (x@(TableDef n _ _ _):xs) = 
    if n == t then Just x else findTable t xs

getColumns :: TableDef -> [ColumnName]
getColumns (TableDef _ a _ _) = map getColumnName a
    
getColumnName :: SqlColumn -> ColumnName
getColumnName (SqlColumn n _ _ _) = n

getTableName :: TableDef -> TableName
getTableName (TableDef n _ _ _) = n

-------------------------------------------------------------------------
-- | s is the string to be converted into an SqlValue
getSqlVal :: SqlColumn -> String -> SqlValue
--getSqlVal (SqlColumn (ColumnName n) t a b)  s | trace ("getSqlVal " ++ n ++ " " ++ s) False = undefined
getSqlVal (SqlColumn _ t _ _) s = 
    let s' = if "" /= s then s else getDefault t
        s'' = if not (null s') && '+' == head s' then tail s' else s'
    in case t of 
         SqlVarCharT  -> SqlString s'
         SqlIntegerT  -> SqlInteger $ read s''
         SqlDecimalT  -> SqlDouble $ read s''
         SqlRealT     -> SqlDouble $ read s''
         SqlBitT      -> SqlBool $ read s'

-------------------------------------------------------------------------
-- | builds an SQL query string for updating a record
bldDelete :: TableName -> WhereClause -> String
bldDelete (TableName t) (WhereClause where_clause) = 
    "DELETE FROM " ++ t ++ " WHERE " ++ where_clause

-------------------------------------------------------------------------
-- | builds an SQL query string for updating a record
bldUpdate :: TableName -> [ColumnName] -> WhereClause -> String
bldUpdate (TableName table) columns (WhereClause where_clause) = 
    "UPDATE " ++ table ++ " SET " ++ update_list ++ " WHERE " ++ where_clause
  where
    updateEntry c = c ++ "=?"
    update_list = if null columns
                  then ""
                  else foldl (\t c -> t ++ ", " ++ (updateEntry $ toStr c)) 
                             (updateEntry $ toStr $ head columns) 
                             (tail columns)

-------------------------------------------------------------------------
-- | returns True if there is an entry in the table where key==value
existsKeyVal :: TableName -> ColumnName -> String -> IO Bool
--existsEntry table key value | trace ("existsEntry " ++ table ++ " " ++ key ++ " " ++ value) False = undefined
existsKeyVal t key value =  do
    results <- runQuery $ bldQueryWhereEq (ColumnName "*") t key $ addQuote value
    return $ "" /= results

-------------------------------------------------------------------------
-- | returns True if there is an entry in the table where key==value
existsByClause :: TableName -> WhereClause -> IO Bool
--existsEntry table key value | trace ("existsEntry " ++ table ++ " " ++ key ++ " " ++ value) False = undefined
existsByClause t clause =  do
    results <- runQuery $ bldQueryWhere (ColumnName "*") t clause
    return $ "" /= results

-------------------------------------------------------------------------
-- | builds a query with a WHERE EQUALS clause
bldQueryWhereEq :: ColumnName -> TableName -> ColumnName -> String -> Maybe String
bldQueryWhereEq col tab key val = 
  bldQueryBasic col tab +++ Just (" WHERE " ++ (toStr $ clauseEQ key val))

bldQueryEq :: ColumnName -> String -> TableName -> ColumnName -> Maybe String
bldQueryEq key val tab col = bldQueryWhereEq col tab key val

getFieldEq :: ColumnName -> String -> TableName -> ColumnName -> IO String 
getFieldEq key val tab col = do
  val <- runQuery $ bldQueryEq key val tab col
  return val

-------------------------------------------------------------------------
-- | builds a query with a WHERE EQUALS clause with multiple keys
bldQueryMultiEq :: ColumnName -> TableName -> [ColumnName] -> [String] -> Maybe String
--bldQueryMultiEq col tab keys vals | trace ("bldQueryMultiEq " ++ show keys ++ "---" ++ show vals) False = undefined
bldQueryMultiEq col tab keys vals = 
  let clauses = zipWith clauseEQ keys $ map addQuote vals
  in  bldQueryBasic col tab +++ Just ( " WHERE " ++ (intercalate " AND " $ map toStr clauses))

clauseEQ :: ColumnName -> String -> WhereClause
clauseEQ (ColumnName a) b = WhereClause $ a ++ "=" ++ b


-------------------------------------------------------------------------
-- | builds a query with a WHERE clause
bldQueryWhere :: ColumnName -> TableName -> WhereClause -> Maybe String
bldQueryWhere col tab (WhereClause whr) = 
  bldQueryBasic col tab +++ Just (" WHERE " ++ whr)

-------------------------------------------------------------------------
-- | builds a query with no clauses
bldQueryBasic :: ColumnName -> TableName -> Maybe String
bldQueryBasic (ColumnName col) (TableName table) = 
  Just "SELECT " +++ notMT col +++ Just " FROM " +++ notMT table

(+++) :: Maybe String -> Maybe String -> Maybe String
(+++) = liftM2 (++)

notMT :: String -> Maybe String
notMT [] = Nothing
notMT x = Just x

-------------------------------------------------------------------------
-- | builds an SQL query string for updating a record
addQuote :: String -> String
addQuote x = "'" ++ x ++ "'"

-------------------------------------------------------------------------
-- | catch a query exception and return the empty list (of results)
--
catchQueryExc :: SqlError -> IO [[SqlValue]]
catchQueryExc x =  do
     putStrLn $ "SQL Error, query is: " ++ show x
     return []

--------------------------------------------------------------------------
-- | runs a query and returns the results in 1 big string, comma separated
runQueryNative :: Maybe String -> IO [[SqlValue]]
runQueryNative Nothing = return []
--runQuery (Just x) | trace x False = undefined
runQueryNative (Just x) =  do
        conn <- connectSqlite3 "dbname=test"
        results <- EXC.catch (quickQuery' conn x []) catchQueryExc
        disconnect conn
        return results

--------------------------------------------------------------------------
-- | runs a query and returns the results in 1 big string, comma separated
runQuery :: Maybe String -> IO String
runQuery x =  do
        q <- runQueryNative x
        return $ (intercalate "," . map fromSql . concat) q

--------------------------------------------------------------------------
-- | runs a query and returns a list of the results
runQueryList :: Maybe String -> IO [String]
runQueryList x =  do
        q <- runQueryNative x
        return $ (map fromSql . concat) q


newtype Query = Query String deriving (Show, Eq)
newtype TableName = TableName String deriving (Show, Eq)
newtype ColumnName = ColumnName String deriving (Show, Eq)
newtype UriKeyName = UriKeyName String deriving (Show, Eq)
newtype WhereClause = WhereClause String deriving (Show, Eq)



getKeyName' :: TableName -> ColumnName -> DT.Text
getKeyName' (TableName table) (ColumnName column) = DT.pack $ table ++ "." ++ column

getKeyName :: TableName -> ColumnName -> UriKeyName
getKeyName (TableName table) (ColumnName column) = UriKeyName $ table ++ "." ++ column

decodeElName :: UriKeyName -> (TableName, ColumnName)
decodeElName (UriKeyName x) = 
   let (a,b) = span (/= '.') x 
   in  if [] == b 
       then (TableName a,ColumnName "")
       else (TableName a,ColumnName (tail b))

buildElName :: TableName -> ColumnName -> UriKeyName
buildElName (TableName t) (ColumnName c) = UriKeyName $ t ++ "." ++ c

getColName :: UriKeyName -> ColumnName
getColName x = let (_,b) = decodeElName x in b

class IsStr a where 
  toStr :: a -> String

instance IsStr TableName where
  toStr (TableName t) = t

instance IsStr ColumnName where
  toStr (ColumnName c) = c

instance IsStr UriKeyName where
  toStr (UriKeyName u) = u

instance IsStr WhereClause where
  toStr (WhereClause u) = u

