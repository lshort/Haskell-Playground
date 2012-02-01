module CreatureTables (
  allCreatureTables, 
  regionTable, rgEnctrTable, levelTable, raceTable, templateTable, 
)  where 

import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Database.HDBC.ColTypes as HDBCCT
import           QueryLite


regionTable :: TableDef
regionTable = TableDef (TableName "region")
    [makeColumn "region_name" SqlVarCharT (Just 24) Nothing
    ,makeColumn "description" SqlVarCharT (Just 32) Nothing
    ,makeColumn "reagents_found" SqlVarCharT (Just 32) Nothing
    ,makeColumn "weather_winter" SqlVarCharT (Just 32) Nothing
    ,makeColumn "weather_spring" SqlVarCharT (Just 32) Nothing
    ,makeColumn "weather_summer" SqlVarCharT (Just 32) Nothing
    ,makeColumn "weather_fall" SqlVarCharT (Just 32) Nothing
    ,makeColumn "median_spotting_dist_ft" SqlIntegerT Nothing Nothing
    ,makeColumn "movement_mult" SqlRealT Nothing Nothing
    ,makeColumn "movement_mult_winter" SqlRealT Nothing Nothing
    ,makeColumn "mtd_chrg_winter_dc" SqlIntegerT Nothing Nothing
    ,makeColumn "mtd_chrg_spring_dc" SqlIntegerT Nothing Nothing
    ,makeColumn "mtd_chrg_summer_dc" SqlIntegerT Nothing Nothing
    ,makeColumn "mtd_chrg_fall_dc" SqlIntegerT Nothing Nothing
    ,makeColumn "surv_winter_dc" SqlIntegerT Nothing Nothing
    ,makeColumn "surv_spring_dc" SqlIntegerT Nothing Nothing
    ,makeColumn "surv_summer_dc" SqlIntegerT Nothing Nothing
    ,makeColumn "surv_fall_dc" SqlIntegerT Nothing Nothing]
    ["region_name"]
    []

rgEnctrTable :: TableDef
rgEnctrTable = TableDef (TableName "region_encounter")
    [makeColumn "encounter_name" SqlVarCharT (Just 24) Nothing
    ,makeColumn "likelihood" SqlIntegerT Nothing Nothing
    ,makeColumn "season" SqlVarCharT (Just 24) Nothing
    ,makeColumn "time_of_day" SqlVarCharT (Just 24) Nothing
    ,makeColumn "other_conditions" SqlVarCharT (Just 32) Nothing
    ,makeColumn "skill0" SqlVarCharT (Just 16) Nothing
    ,makeColumn "skill0_dc" SqlIntegerT Nothing Nothing
    ,makeColumn "skill0_consequences" SqlVarCharT (Just 32) Nothing
    ,makeColumn "skill1" SqlVarCharT (Just 16) Nothing
    ,makeColumn "skill1_dc" SqlIntegerT Nothing Nothing
    ,makeColumn "skill1_consequences" SqlVarCharT (Just 32) Nothing
    ,makeColumn "region_name" SqlVarCharT (Just 24) Nothing
    ,makeColumn "race_0" SqlVarCharT (Just 24) Nothing
    ,makeColumn "template_0" SqlVarCharT (Just 24) Nothing
    ,makeColumn "number_base_0" SqlIntegerT Nothing Nothing
    ,makeColumn "dice_size_0" SqlIntegerT Nothing Nothing
    ,makeColumn "dice_count_0" SqlIntegerT Nothing Nothing
    ,makeColumn "race_1" SqlVarCharT (Just 24) Nothing
    ,makeColumn "template_1" SqlVarCharT (Just 24) Nothing
    ,makeColumn "number_base_1" SqlIntegerT Nothing Nothing
    ,makeColumn "dice_size_1" SqlIntegerT Nothing Nothing
    ,makeColumn "dice_count_1" SqlIntegerT Nothing Nothing
    ,makeColumn "race_2" SqlVarCharT (Just 24) Nothing
    ,makeColumn "template_2" SqlVarCharT (Just 24) Nothing
    ,makeColumn "number_base_2" SqlIntegerT Nothing Nothing
    ,makeColumn "dice_size_2" SqlIntegerT Nothing Nothing
    ,makeColumn "dice_count_2" SqlIntegerT Nothing Nothing
    ,makeColumn "race_3" SqlVarCharT (Just 24) Nothing
    ,makeColumn "template_3" SqlVarCharT (Just 24) Nothing
    ,makeColumn "number_base_3" SqlIntegerT Nothing Nothing
    ,makeColumn "dice_size_3" SqlIntegerT Nothing Nothing
    ,makeColumn "dice_count_3" SqlIntegerT Nothing Nothing
    ,makeColumn "race_4" SqlVarCharT (Just 24) Nothing
    ,makeColumn "template_4" SqlVarCharT (Just 24) Nothing
    ,makeColumn "number_base_4" SqlIntegerT Nothing Nothing
    ,makeColumn "dice_size_4" SqlIntegerT Nothing Nothing
    ,makeColumn "dice_count_4" SqlIntegerT Nothing Nothing
    ]
    ["encounter_name", "region_name"]
    [( "region_name","region(region_name)" )]

levelTable :: TableDef
levelTable = TableDef  (TableName "level")
    [makeColumn "level" SqlIntegerT Nothing Nothing
    ,makeColumn "XP" SqlIntegerT Nothing Nothing
    ,makeColumn "HD" SqlIntegerT Nothing Nothing
    ,makeColumn "hd_size" SqlIntegerT Nothing Nothing
    ,makeColumn "damage_dice" SqlIntegerT Nothing Nothing
    ,makeColumn "damage_size" SqlIntegerT Nothing Nothing]
    ["Level"]
    []

raceTable :: TableDef
raceTable = TableDef  (TableName "race")
    [makeColumn "race_name" SqlVarCharT (Just 24) Nothing
    ,makeColumn "pri_melee_wpn" SqlVarCharT (Just 16) Nothing
    ,makeColumn "pri_melee_bonus" SqlIntegerT Nothing Nothing
    ,makeColumn "pri_melee_dmg" SqlIntegerT Nothing Nothing
    ,makeColumn "sec_melee_wpn" SqlVarCharT (Just 16) Nothing
    ,makeColumn "sec_melee_bonus" SqlIntegerT Nothing Nothing
    ,makeColumn "sec_melee_dmg" SqlIntegerT Nothing Nothing
    ,makeColumn "elt_melee_wpn" SqlVarCharT (Just 16) Nothing
    ,makeColumn "elt_melee_bonus" SqlIntegerT Nothing Nothing
    ,makeColumn "elt_melee_dmg" SqlIntegerT Nothing Nothing
    ,makeColumn "pri_range_wpn" SqlVarCharT (Just 16) Nothing
    ,makeColumn "pri_range_bonus" SqlIntegerT Nothing Nothing
    ,makeColumn "pri_range_dmg" SqlIntegerT Nothing Nothing
    ,makeColumn "sec_range_wpn" SqlVarCharT (Just 16) Nothing
    ,makeColumn "sec_range_bonus" SqlIntegerT Nothing Nothing
    ,makeColumn "sec_range_dmg" SqlIntegerT Nothing Nothing
    ,makeColumn "elt_range_wpn" SqlVarCharT (Just 16) Nothing
    ,makeColumn "elt_range_bonus" SqlIntegerT Nothing Nothing
    ,makeColumn "elt_range_dmg" SqlIntegerT Nothing Nothing
    ,makeColumn "xp_level" SqlIntegerT Nothing Nothing
    ,makeColumn "move" SqlIntegerT Nothing Nothing
    ,makeColumn "mod_str" SqlIntegerT Nothing Nothing
    ,makeColumn "mod_int" SqlIntegerT Nothing Nothing
    ,makeColumn "mod_wis" SqlIntegerT Nothing Nothing
    ,makeColumn "mod_dex" SqlIntegerT Nothing Nothing
    ,makeColumn "mod_con" SqlIntegerT Nothing Nothing
    ,makeColumn "mod_cha" SqlIntegerT Nothing Nothing
    ,makeColumn "prime" SqlVarCharT (Just 8) Nothing
    ,makeColumn "armor_class" SqlIntegerT Nothing Nothing
    ,makeColumn "hd_adjustment" SqlIntegerT Nothing Nothing
    ,makeColumn "special_abilities" SqlVarCharT (Just 128) Nothing
    ]
    ["Race_name"]
    [( "xp_level","level(level)" )]

templateTable :: TableDef
templateTable = TableDef  (TableName "template")
    [makeColumn "tpl_name" SqlVarCharT (Just 24) Nothing
    ,makeColumn "armor_class" SqlIntegerT Nothing Nothing
    ,makeColumn "hit_dice" SqlIntegerT Nothing Nothing
    ,makeColumn "mod_str" SqlIntegerT Nothing Nothing
    ,makeColumn "mod_int" SqlIntegerT Nothing Nothing
    ,makeColumn "mod_wis" SqlIntegerT Nothing Nothing
    ,makeColumn "mod_dex" SqlIntegerT Nothing Nothing
    ,makeColumn "mod_con" SqlIntegerT Nothing Nothing
    ,makeColumn "mod_cha" SqlIntegerT Nothing Nothing
    ,makeColumn "prime_override" SqlVarCharT (Just 8) Nothing
    ,makeColumn "level_adjustment" SqlIntegerT Nothing Nothing
    ,makeColumn "special_abilities" SqlVarCharT (Just 128) Nothing
    ,makeColumn "melee_wpn" SqlVarCharT (Just 12) Nothing
    ,makeColumn "missile_wpn" SqlVarCharT (Just 12) Nothing
    ]
    ["tpl_name"]
    []

allCreatureTables :: [TableDef]
allCreatureTables = [  regionTable, rgEnctrTable, levelTable, raceTable, templateTable ]

