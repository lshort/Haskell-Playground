 <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" 
"http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
    <title>TEMPLATES</title>
    <link rel="stylesheet" type="text/css" href="screen.css" />
</head>
<body>

<!-- template info -->
<div>
<strong>SELECT</strong>
<sql_drop_w_save  name="old_name" value="uri:template.tpl_name" column="tpl_name" table="template" action="template_template" />
</div>
<div>
<form method="post" name="dataform" action="template_template?_task=save">
<fieldset>
<legend><strong>EDIT</strong></legend>
<uri_text_input name="template.tpl_name" value="uri:template.tpl_name" label="Name" />
<br />
<sql_text_input column="special_abilities" label="Special" table="template" key="tpl_name" keyval="uri:template.tpl_name" size="70" />
</fieldset>
<fieldset>
<legend>PRIME OVERRIDE</legend>
<sql_radio column="prime_override" value="None" table="template" key="tpl_name" keyval="uri:template.tpl_name" />
<sql_radio column="prime_override" value="Mental" table="template" key="tpl_name" keyval="uri:template.tpl_name" />
<sql_radio column="prime_override" value="Physical" table="template" key="tpl_name" keyval="uri:template.tpl_name" />
</fieldset>
<fieldset>
<legend>ADJUSTMENTS</legend>
<sql_text_input column="level_adjustment" label="Level" table="template" key="tpl_name" keyval="uri:template.tpl_name" size="3" />
<sql_text_input column="hit_dice" label="Hit Dice" table="template" key="tpl_name" keyval="uri:template.tpl_name" size="3" />
<sql_text_input column="armor_class" label="AC" table="template" key="tpl_name" keyval="uri:template.tpl_name" size="3" />
<br />
<sql_text_input column="mod_str" label="Str" table="template" key="tpl_name" keyval="uri:template.tpl_name" size="3" />
<sql_text_input column="mod_dex" label="Dex" table="template" key="tpl_name" keyval="uri:template.tpl_name" size="3" />
<sql_text_input column="mod_con" label="Con" table="template" key="tpl_name" keyval="uri:template.tpl_name" size="3" />
<br />
<sql_text_input column="mod_int" label="Int" table="template" key="tpl_name" keyval="uri:template.tpl_name" size="3" />
<sql_text_input column="mod_wis" label="Wis" table="template" key="tpl_name" keyval="uri:template.tpl_name" size="3" />
<sql_text_input column="mod_cha" label="Cha" table="template" key="tpl_name" keyval="uri:template.tpl_name" size="3" />
</fieldset>
<fieldset>
<legend>WEAPONS</legend>
MELEE
<sql_radio column="melee_wpn" value="Secondary" table="template" key="tpl_name" keyval="uri:template.tpl_name" />
<sql_radio column="melee_wpn" value="Primary" table="template" key="tpl_name" keyval="uri:template.tpl_name" />
<sql_radio column="melee_wpn" value="Elite" table="template" key="tpl_name" keyval="uri:template.tpl_name" />
<br />
MISSILE
<sql_radio column="missile_wpn" value="Secondary" table="template" key="tpl_name" keyval="uri:template.tpl_name" />
<sql_radio column="missile_wpn" value="Primary" table="template" key="tpl_name" keyval="uri:template.tpl_name" />
<sql_radio column="missile_wpn" value="Elite" table="template" key="tpl_name" keyval="uri:template.tpl_name" />
</fieldset>
<input type="submit" name="save" value="Save">
<input type="submit" name="delete" value="Delete">
</form>
</div>


<apply template="nav"/>
</body>
</html>
