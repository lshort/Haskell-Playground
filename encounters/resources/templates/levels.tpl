<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" 
"http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
    <title>LEVELS</title>
    <link rel="stylesheet" type="text/css" href="screen.css" />
</head>

<body>

<div>
<strong>SELECT</strong>
<!-- template info -->
<sql_drop_w_save  name="old_level" value="uri:level.level" column="level" table="level" action="level_template" />
</div>
<div>
<form method="post" name="dataform" action="level_template?_task=save">
<fieldset>
<legend><strong>EDIT</strong></legend>
<uri_text_input name="level.level" value="uri:level.level" label="Level" />
<sql_text_input column="XP" label="XP Value" table="level" key="level" keyval="uri:level.level" size="10" />
<sql_text_input column="HD" label="Hit Dice" table="level" key="level" keyval="uri:level.level" size="4" />
<sql_text_input column="hd_size" label="Die Size" table="level" key="level" keyval="uri:level.level" size="4" />
<sql_text_input column="damage_dice" label="Damage Dice" table="level" key="level" keyval="uri:level.level" size="4" />
<sql_text_input column="damage_size" label="Die Size" table="level" key="level" keyval="uri:level.level" size="4" />
</fieldset>
<input type="submit" name="save" value="Save">
<input type="submit" name="delete" value="Delete">
</form>
</div>

<apply template="nav"/>
</body>
</html>
