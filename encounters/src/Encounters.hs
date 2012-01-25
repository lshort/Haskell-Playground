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

repeatM :: Monad m => (a -> m b) -> Int -> a -> m [b]
repeatM f n x = mapM f (replicate n x)

rollDie :: Int -> IO Int
rollDie sides = getStdRandom (randomR (1,sides))

rollDice :: Int -> Int -> IO Int
rollDice count sides = liftM sum $ repeatM rollDie count sides

getHitDice :: String -> String -> IO (Int, Int)
getHitDice race template = do
    let getRace = getFieldEq (ColumnName "race_name") (addQuote race) (TableName "race") . ColumnName
        getTpl = getFieldEq (ColumnName "tpl_name") (addQuote template) (TableName "template") . ColumnName
    base_lvl <- getRace "xp_level"
    lvl_mod <- getTpl "level_adjustment"
    let level = (read lvl_mod) + (read base_lvl) :: Int
        getLvl = getFieldEq (ColumnName "level") (show level) (TableName "level") . ColumnName
    hitDice <- getLvl "HD"
    hdSize  <- getLvl "hd_size"
    hitDieAdj <- getRace "hd_adjustment"
    hitDieAd  <- getTpl "hit_dice"
    return $ (read hitDice + read hitDieAdj + read hitDieAd, read hdSize)

getAttr :: (String -> IO String) -> (String -> IO String) -> String -> String -> Int -> Bool -> String -> IO Int
getAttr f g prime over hd phys x = do
    fx <- f x
    gx <- g x
    let physPrime = (prime == "Physical" && over /= "Mental" ) 
                    || (over == "Physical")
        isPrime = physPrime == phys
        bonus = if isPrime then 4 else 0
    return $ read fx + read gx + bonus + hd

getBase :: (String -> IO String) -> (String -> IO String) -> String -> IO Int
getBase f g  x = do
    fx <- f x
    gx <- g x
    return $ read fx + read gx

buildStatLine :: String -> String -> IO String
buildStatLine race template = do
    let getRace = getFieldEq (ColumnName "race_name") (addQuote race) (TableName "race") . ColumnName
        getTpl = getFieldEq (ColumnName "tpl_name") (addQuote template) (TableName "template") . ColumnName
    base_lvl <- getRace "xp_level"
    lvl_mod <- getTpl "level_adjustment"
    let level = (read lvl_mod) + (read base_lvl) :: Int
        getLvl = getFieldEq (ColumnName "level") (show level) (TableName "level") . ColumnName
    (hitDice, hitDieSize) <- getHitDice race template
    prime <- getRace "prime"
    override <- getTpl "prime_override"
    let getA = getAttr getRace getTpl prime override hitDice
    str <- getA True  "mod_str"
    dex <- getA True  "mod_dex"
    con <- getA True  "mod_con"
    int <- getA False "mod_int"
    wis <- getA False "mod_wis"
    cha <- getA False "mod_cha"
    ac <- getBase getRace getTpl "armor_class"
    strB <- getBase getRace getTpl "mod_str"
    dexB <- getBase getRace getTpl "mod_dex"
    move <- getRace "move"
    specialRace <- getRace "special_abilities"
    specialTpl <- getTpl "special_abilities"
    wpns <- wpnInfo getRace getTpl level strB dexB hitDice
    return $ (metaC race) ++ " " ++ (metaC template) ++ " AC" ++ (show ac)
             ++ " Tc" ++ show (10 + dexB)
             ++ " Grp" ++ show (10 + dexB + strB)
             ++ " HD" ++ (show hitDice)
             ++ " Mv" ++ move ++ " sv(S" ++ (show str) ++ ".I" ++ (show int) 
             ++ ".W" ++ (show wis) ++ ".D" ++ (show dex) ++ ".C" ++ (show con) 
             ++ ".Ch" ++ (show cha ) ++ ") " ++ wpns 
             ++ showSpecial specialRace specialTpl
  where
    metaC xs = (toUpper $ head xs) : tail xs
    showSpecial x y 
       | null x && null y  = ""
       | null x            = "\n        Special: " ++ y
       | null y            = "\n        Special: " ++ x
       | otherwise         = "\n        Special: " ++ x ++ ", " ++ y

wpnInfo :: (String -> IO String) -> (String -> IO String) -> Int -> Int -> Int -> Int -> IO String
wpnInfo getRace getTpl level strB dexB hitDice = do 
    melee <- getTpl "melee_wpn" -- secondary/primary/elite
    ranged <- getTpl "missile_wpn"
    mWpn <- getRace $ get_abbrev melee ++ "_melee_wpn"
    mDmg <- getRace $ get_abbrev melee ++ "_melee_dmg"
    mBon <- getRace $ get_abbrev melee ++ "_melee_bonus"
    rWpn <- getRace $ get_abbrev ranged ++ "_range_wpn"
    rDmg <- getRace $ get_abbrev ranged ++ "_range_dmg"
    rBon <- getRace $ get_abbrev ranged ++ "_range_bonus"
    mDamageFull <- getDmg level mDmg
    rDamageFull <- getDmg level rDmg
    let mBonus = (read mBon) + hitDice
        rBonus = (read rBon) + hitDice
    return $ "M[" ++ (formatAtk mWpn mBonus mDamageFull) ++ "] R[" ++ (formatAtk rWpn rBonus rDamageFull) ++ "]"
  where 
    get_abbrev x
       | x == "Secondary"  = "sec"
       | x == "Elite"      = "elt"
       | otherwise         = "pri"

formatAtk :: String -> Int -> String -> String
formatAtk w b d = if null w then "" 
                            else w ++ "+" ++ (show b) ++ "/" ++ d

getDmg :: Int -> String -> IO String
getDmg lvl mod = do
    let level = lvl + (read mod)
        getLvl = getFieldEq (ColumnName "level") (show level) (TableName "level") . ColumnName
    dice <- getLvl "damage_dice"
    size <- getLvl "damage_size"
    return $ dice ++ "d" ++ size


buildHP :: String -> String -> String -> String -> String -> IO String
buildHP race tpl base ds dc = do
    dice <- rollDice (read dc) (read ds)
    let count = (read base) + dice
    (hitDice, hitDieSize) <- getHitDice race tpl
    list <- repeatM (rollDice hitDice) count hitDieSize
    return $ "HP: " ++ (intercalate ",  " $ map show list)

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
    return $ (map toUpper encounter) ++ "  " ++ wrap season ++ wrap time ++ wrap cond ++ 
             "  " ++ format sk0 sk0dc sk0con ++ format sk1 sk1dc sk1con
  where
    wrap s = if null s then "" else "[" ++ s ++ "]"
    format name dc con = if null name 
                          then "" 
                          else "(" ++ name ++ "|DC" ++ dc ++ "|" ++ con ++ ")"

creatureStrings :: String -> String -> String -> String -> String -> IO [String]
creatureStrings race tpl base ds dc = do
    stats <- buildStatLine race tpl
    hp <- buildHP race tpl base ds dc
    return $ [stats,"   " ++ hp]

 
buildCreature :: String -> String -> Int -> IO [String]
buildCreature region encounter number = do
    let myField = getRegionField region encounter
    race <- myField $ "race_" ++ show number
    template <- myField $ "template_" ++ show number
    base <- myField $ "number_base_" ++ show number
    dieSize <- myField $ "dice_size_" ++ show number
    dieCount <- myField $ "dice_count_" ++ show number
    if race == "" 
       then return []
       else creatureStrings race template base dieSize dieCount

buildCreatures :: String -> String -> IO String
buildCreatures region encounter = do
    strings <- mapM (buildCreature region encounter) [0..4]
    return $ intercalate "\n     " $ [""] ++ (takeWhile (/="") $ concat strings)

buildNthEncounter :: Int -> String -> IO String
--buildNthEncounter n region | trace "buildNthEncounter" False = undefined
buildNthEncounter n region = do
    qval <- runQueryNative $ bldQueryWhereEq (ColumnName "*") (TableName "region_encounter") 
                                             (ColumnName "region_name") $ addQuote region
    let enc = qval !! n
        encName = fromSql $ head enc
    header <- buildHeader region encName 
    creatures <- buildCreatures region encName
    return $ header ++ creatures

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
  | head xs == ""    = toLines (tail xs) (addTic acc)
  | null acc         = head xs : toLines (tail xs) ""
  | otherwise        = acc : head xs : toLines (tail xs) ""

addTic :: String -> String
addTic xs = 
    if null xs || 10 /= (length xs) `mod` 14
       then xs ++ "O "
       else xs ++ "X.  "

printEncs :: String -> Int -> IO ()
printEncs region pages = do 
    let encounters = map (unsafePerformIO . buildEnc) $ repeat region
    replicateM_ pages $ printPage region encounters

printPage :: String -> [String] -> IO ()
printPage region encounters = do 
    printRegion region
    putStrLn "ENCOUNTERS"
    printEncs' (toLines encounters "") 45
  where
    printEncs' :: [String] -> Int -> IO ()
    printEncs' _ 0 = return ()
    printEncs' es x = do
      printI $ head es
      printEncs' (tail es) (x-1)

printI :: String -> IO ()
printI x = putStrLn $ "   " ++ x

printRegion :: String -> IO ()
printRegion region = do
    let getRegion = getFieldEq (ColumnName "region_name") (addQuote region) (TableName "region") . ColumnName
    description <- getRegion "description"
    reagents <- getRegion "reagents_found"
    spot   <- getRegion "median_spotting_dist_ft"
    move   <- getRegion "movement_mult"
    moveW  <- getRegion "movement_mult_winter"
    winter <- getRegion "weather_winter"
    spring <- getRegion "weather_spring"
    summer <- getRegion "weather_summer"
    fall   <- getRegion "weather_fall"
    winterSu <- getRegion "surv_winter_dc"
    winterCh <- getRegion "mtd_chrg_winter_dc"
    summerSu <- getRegion "surv_summer_dc"
    summerCh <- getRegion "mtd_chrg_summer_dc"
    springSu <- getRegion "surv_spring_dc"
    springCh <- getRegion "mtd_chrg_spring_dc"
    fallSu   <- getRegion "surv_fall_dc"
    fallCh   <- getRegion "mtd_chrg_fall_dc"
    putStrLn $ region ++ "   [reagents: " ++ reagents ++ "]"
    printI $ "Median Spotting Distance: " ++ spot
    printI $ "WINTER: " ++ winter ++ " [surv:" ++ winterSu ++ "] {mtd chg:" 
             ++ winterCh ++ "} (move:" ++ moveW ++ "x)"
    printI $ "SPRING: " ++ spring ++ " [surv:" ++ springSu ++ "] {mtd chg:" 
             ++ springCh ++ "}"
    printI $ "SUMMER: " ++ summer ++ " [surv:" ++ summerSu ++ "] {mtd chg:" 
             ++ summerCh ++ "} (move:" ++ moveW ++ "x)"
    printI $ "FALL: " ++ fall ++ " [surv:" ++ fallSu ++ "] {mtd chg:" 
             ++ fallCh ++ "}"


main = do
    args <- getArgs
    let region = head args
        pages = readDef 1 $ headDef "1" $ tail args
    printEncs region pages
