 <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" 
"http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
    <title>CREATURES</title>
    <link rel="stylesheet" type="text/css" href="screen.css" />
</head>

<body>

<!-- race info -->
<div>
<strong>SELECT</strong>
<sql_drop_w_save  name="old_name" value="uri:race.race_name" column="race_name" table="race" action="creature_template" />
</div>
<div>
<form method="post" name="dataform" action="creature_template?_task=save">
<strong>EDIT</strong>
<div>
<p>
<uri_text_input name="race.race_name" value="uri:race.race_name" label="Name" />
<br />
<sql_text_input column="special_abilities" label="Special" table="race" key="race_name" keyval="uri:race.race_name" size="70" />
<br />
<sql_text_input column="xp_level" label="Level" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<sql_text_input column="move" label="Move" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<sql_text_input column="hd_adjustment" label="HD Adjustment" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<sql_text_input column="armor_class" label="AC" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
</p>
</div>
<div>
<p>
<strong>Attributes</strong>
<br />
Prime: 
<sql_radio column="prime" value="Mental" table="race" key="race_name" keyval="uri:race.race_name" />
<sql_radio column="prime" value="Physical" table="race" key="race_name" keyval="uri:race.race_name" />
<br />
<sql_text_input column="mod_str" label="Str" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<sql_text_input column="mod_dex" label="Dex" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<sql_text_input column="mod_con" label="Con" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<sql_text_input column="mod_int" label="Int" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<sql_text_input column="mod_wis" label="Wis" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<sql_text_input column="mod_cha" label="Cha" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
</p>
</div>
<div>
<p>
<strong>Melee</strong>
<br />
Primary
<sql_text_input column="pri_melee_wpn" label="Wpn" table="race" key="race_name" keyval="uri:race.race_name" size="16" />
<sql_text_input column="pri_melee_bonus" label="Atk" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<sql_text_input column="pri_melee_dmg" label="Con" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<br />
Secondary
<sql_text_input column="sec_melee_wpn" label="Wpn" table="race" key="race_name" keyval="uri:race.race_name" size="16" />
<sql_text_input column="sec_melee_bonus" label="Atk" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<sql_text_input column="sec_melee_dmg" label="Dmg" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<br />
Elite
<sql_text_input column="elt_melee_wpn" label="Wpn" table="race" key="race_name" keyval="uri:race.race_name" size="16" />
<sql_text_input column="elt_melee_bonus" label="Atk" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<sql_text_input column="elt_melee_dmg" label="Dmg" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
</p>
</div>
<div>
<p>
<strong>Ranged</strong>
<br />
Primary
<sql_text_input column="pri_range_wpn" label="Wpn" table="race" key="race_name" keyval="uri:race.race_name" size="16" />
<sql_text_input column="pri_range_bonus" label="Atk" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<sql_text_input column="pri_range_dmg" label="Con" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<br />
Secondary
<sql_text_input column="sec_range_wpn" label="Wpn" table="race" key="race_name" keyval="uri:race.race_name" size="16" />
<sql_text_input column="sec_range_bonus" label="Atk" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<sql_text_input column="sec_range_dmg" label="Dmg" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<br />
Elite
<sql_text_input column="elt_range_wpn" label="Wpn" table="race" key="race_name" keyval="uri:race.race_name" size="16" />
<sql_text_input column="elt_range_bonus" label="Atk" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
<sql_text_input column="elt_range_dmg" label="Dmg" table="race" key="race_name" keyval="uri:race.race_name" size="3" />
</p>
</div>

<input type="submit" name="save" value="Save">
<input type="submit" name="delete" value="Delete">
</form>
</div>
<apply template="nav"/>
</body>
</html>
