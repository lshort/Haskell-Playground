{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

{-|
This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.
-}

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
import           System.Environment
import           Debug.Trace
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           System.Random
import           System.IO.Unsafe

rollDie :: Int -> IO Int
rollDie sides = getStdRandom (randomR (1,sides))


buildStatLine :: String -> String -> IO String
buildStatLine race template = do
    let getRace = getFieldEq (ColumnName "race_name") race (TableName "race") . ColumnName
        getTpl = getFieldEq (ColumnName "tpl_name") template (TableName "template") . ColumnName
    base_lvl <- getRace "xp_level"
    lvl_mod <- getTpl "level_adjustment"
    let level = (read lvl_mod) + (read base_lvl) :: Int
        getLvl = getFieldEq (ColumnName "level") (show level) (TableName "level") . ColumnName
    return ""

selectEnc :: [String] -> IO Int
selectEnc xs = do
    let counts = map read xs
    die <- rollDie $ sum counts
    return $ reduce counts die 0
  where
    reduce ys dec acc = if dec <= head ys
                           then acc
                           else reduce (tail ys) (dec - head ys) (acc + 1)

getRegionField :: String -> String -> String -> IO String
getRegionField region encounter field = do
    qval <- runQuery $ bldQueryMultiEq 
                         (ColumnName field) (TableName "region_encounter")
                         [ColumnName "region_name", ColumnName "encounter_name"]
                         [region, encounter] 
    return qval

buildHeader :: String -> String -> IO String
buildHeader region encounter = do
    let myField = getRegionField region encounter
    season <- myField "season"
    time <- myField "time_of_day"
    cond <- myField "other_conditions"
    sk0 <- myField "skill0"
    sk0dc <- myField "skill0_dc"
    sk0con <- myField "skill0_consequences"
    sk1 <- myField "skill1"
    sk1dc <- myField "skill1_dc"
    sk1con <- myField "skill1_consequences"
    return $ encounter ++ "  " ++ wrap season ++ wrap time ++ wrap cond ++ 
             "  " ++ format sk0 sk0dc sk0con ++ format sk1 sk1dc sk1con
  where
    wrap s = if null s then "" else "[" ++ s ++ "]"
    format name dc con = if null name 
                          then "" 
                          else "(" ++ name ++ "/DC:" ++ dc ++ "/" ++ con ++ ")"

creatureString :: String -> String -> String -> String -> String -> IO String
creatureString race tpl base ds dc = undefined
 
buildCreature :: String -> String -> Int -> IO String
buildCreature region encounter number = do
    let myField = getRegionField region encounter
    race <- myField $ "race_" ++ show number
    template <- myField $ "template_" ++ show number
    base <- myField $ "base_number_" ++ show number
    dieSize <- myField $ "dice_size_" ++ show number
    dieCount <- myField $ "dice_count_" ++ show number
    if race == "" 
       then return ""
       else creatureString race template base dieSize dieCount

buildCreatures :: String -> String -> IO String
buildCreatures region encounter = do
    strings <- mapM (buildCreature region encounter) [0..4]
    return $ intercalate "\n" $ takeWhile (/="") strings

buildNthEncounter :: Int -> String -> IO String
--buildNthEncounter n region | trace "buildNthEncounter" False = undefined
buildNthEncounter n region = do
    qval <- runQueryNative $ bldQueryWhereEq (ColumnName "*") (TableName "region_encounter") 
                                             (ColumnName "region_name") $ addQuote region
    let enc = qval !! n
        encName = fromSql $ head enc
    header <- buildHeader region encName 
    creatures <- buildCreatures region encName
    return $ header ++ "\n" ++ creatures

buildEnc :: String -> IO String
--buildEnc region | trace "buildEnc" False = undefined
buildEnc region = do
    die <- rollDie 24
    if (1 /= die)
       then return ""
       else do
            qval <- runQueryList $ bldQueryWhereEq (ColumnName "likelihood") (TableName "region_encounter") 
                                                   (ColumnName "region_name") $ addQuote region
            encounter <- selectEnc qval
            text <- buildNthEncounter encounter region
            return text

toLines :: [String] -> String -> [String]
--toLines xs acc | trace "toLines" False = undefined
toLines xs acc
  | null xs          = [acc]
  | head xs == ""    = toLines (tail xs) ("O " ++ acc)
  | null acc         = head xs : toLines (tail xs) ""
  | otherwise        = acc : head xs : toLines (tail xs) ""

printEncs :: String -> Int -> IO ()
printEncs region pages = do 
    let encounters = map (unsafePerformIO . buildEnc) $ repeat region
    printEncs' (toLines encounters "") (40 * pages)
  where
    printEncs' :: [String] -> Int -> IO ()
    printEncs' _ 0 = return ()
    printEncs' es x = do
      print $ head es
      printEncs' (tail es) (x-1)

main = do
    args <- getArgs
    let region = head args
        pages = readDef 1 $ headDef "1" $ tail args
    printEncs region pages
