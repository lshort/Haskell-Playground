<div id="region">
<div class="subdiv">
<strong>SELECT</strong>
<sql_drop_w_save  name="old_region" value="uri:region.region_name" column="region_name" table="region" action="region_template" save_task="save_region" />
</div>
<div class="subdiv">
<form method="post" name="dataform" action="region_template?_task=save_region">
<fieldset>
<legend><strong>EDIT</strong></legend>
<uri_text_input name="region.region_name" value="uri:region.region_name" label="Region Name" />
<br />
<sql_text_input column="description" label="Description" table="region" key="region_name" keyval="uri:region.region_name" size="60" />
<br />
<sql_text_input column="reagents_found" label="Reagents" table="region" key="region_name" keyval="uri:region.region_name" size="45" />
</fieldset>
<fieldset>
<legend>WEATHER</legend>
<sql_text_input column="weather_winter" label="Winter" table="region" key="region_name" keyval="uri:region.region_name" size="20" />
<sql_text_input column="weather_spring" label="Spring" table="region" key="region_name" keyval="uri:region.region_name" size="20" />
<br />
<sql_text_input column="weather_summer" label="Summer" table="region" key="region_name" keyval="uri:region.region_name" size="20" />
<sql_text_input column="weather_fall" label="Fall" table="region" key="region_name" keyval="uri:region.region_name" size="20" />
<br />
<sql_text_input column="median_spotting_dist_ft" label="Median Spot Distance Ft" table="region" key="region_name" keyval="uri:region.region_name" size="6" />
</fieldset>
<fieldset>
<legend>MOVEMENT MULTs</legend>
<sql_text_input column="movement_mult" label="Regular" table="region" key="region_name" keyval="uri:region.region_name" size="6" />
<sql_text_input column="movement_mult_winter" label="Winter" table="region" key="region_name" keyval="uri:region.region_name" size="6" />
</fieldset>
<fieldset>
<legend>MOUNTED CHARGE DCs</legend>
<sql_text_input column="mtd_chrg_winter_dc" label="Winter" table="region" key="region_name" keyval="uri:region.region_name" size="6" />
<sql_text_input column="mtd_chrg_spring_dc" label="Spring" table="region" key="region_name" keyval="uri:region.region_name" size="6" />
<sql_text_input column="mtd_chrg_summer_dc" label="Summer" table="region" key="region_name" keyval="uri:region.region_name" size="6" />
<sql_text_input column="mtd_chrg_fall_dc" label="Fall" table="region" key="region_name" keyval="uri:region.region_name" size="6" />
</fieldset>
<fieldset>
<legend>SURVIVAL DCs</legend>
<sql_text_input column="surv_winter_dc" label="Winter" table="region" key="region_name" keyval="uri:region.region_name" size="6" />
<sql_text_input column="surv_spring_dc" label="Spring" table="region" key="region_name" keyval="uri:region.region_name" size="6" />
<sql_text_input column="surv_summer_dc" label="Summer" table="region" key="region_name" keyval="uri:region.region_name" size="6" />
<sql_text_input column="surv_fall_dc" label="Fall" table="region" key="region_name" keyval="uri:region.region_name" size="6" />
</fieldset>
<input type="submit" name="save_region" value="Save">
<input type="submit" name="delete_region" value="Delete">
</form>
</div>
</div>