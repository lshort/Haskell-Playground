
<div id="encounter">
<div class="subdiv" >
<strong>SELECT</strong>
<sql_drop_w_save  name="old_region_encounter" value="uri:region_encounter.encounter_name" column="encounter_name" table="region_encounter" action="region_template" keys="1" key1="region_name" keyval1="uri:region.region_name" save_task="save_region_encounter">
    <uri_hidden_input name="region.region_name" value="uri:region.region_name" />
</sql_drop_w_save>
</div>
<div class="subdiv">
<form method="post" name="dataform" action="region_template?_task=save_region_encounter">
<uri_hidden_input name="region.region_name" value="uri:region.region_name" />
<fieldset>
<legend><strong>EDIT</strong></legend>
<uri_text_input name="region_encounter.region_name" value="uri:region.region_name" label="Region Name" />
<br />
<uri_text_input name="region_encounter.encounter_name" value="uri:region_encounter.encounter_name" label="Encounter Name" />
<br />
<sql_text_input column="likelihood" label="Likelihood" table="region_encounter" keys="2" key2="encounter_name" keyval2="uri:region_encounter.encounter_name" key1="region_name" keyval1="uri:region.region_name" size="5" />
<sql_text_input column="season" label="Season" table="region_encounter" keys="2" key2="encounter_name" keyval2="uri:region_encounter.encounter_name" key1="region_name" keyval1="uri:region.region_name" size="25" />
<br />
<sql_text_input column="time_of_day" label="Time Of Day" table="region_encounter" keys="2" key2="encounter_name" keyval2="uri:region_encounter.encounter_name" key1="region_name" keyval1="uri:region.region_name" size="25" />
<sql_text_input column="other_conditions" label="Other Conditions" table="region_encounter" keys="2" key2="encounter_name" keyval2="uri:region_encounter.encounter_name" key1="region_name" keyval1="uri:region.region_name" size="33" />
<br />
<sql_text_input column="skill0" label="Skill" table="region_encounter" keys="2" key2="encounter_name" keyval2="uri:region_encounter.encounter_name" key1="region_name" keyval1="uri:region.region_name" size="15" />
<sql_text_input column="skill0_dc" label="DC" table="region_encounter" keys="2" key2="encounter_name" keyval2="uri:region_encounter.encounter_name" key1="region_name" keyval1="uri:region.region_name" size="4" />
<sql_text_input column="skill0_consequences" label="Failure" table="region_encounter" keys="2" key2="encounter_name" keyval2="uri:region_encounter.encounter_name" key1="region_name" keyval1="uri:region.region_name" size="31" />
<br />
<sql_text_input column="skill1" label="Skill" table="region_encounter" keys="2" key2="encounter_name" keyval2="uri:region_encounter.encounter_name" key1="region_name" keyval1="uri:region.region_name" size="15" />
<sql_text_input column="skill1_dc" label="DC" table="region_encounter" keys="2" key2="encounter_name" keyval2="uri:region_encounter.encounter_name" key1="region_name" keyval1="uri:region.region_name" size="4" />
<sql_text_input column="skill1_consequences" label="Failure" table="region_encounter" keys="2" key2="encounter_name" keyval2="uri:region_encounter.encounter_name" key1="region_name" keyval1="uri:region.region_name" size="31" />
</fieldset>
<fieldset>
<legend>CREATURES</legend>
<table>
  <tr>
    <th>Species</th> <th>Template</th> <th>Base Number</th> <th># Dice</th> <th>Die Size</th>
  </tr>
  <apply template="creature_row"><bind tag="count">0</bind></apply>
  <apply template="creature_row"><bind tag="count">1</bind></apply>
  <apply template="creature_row"><bind tag="count">2</bind></apply>
  <apply template="creature_row"><bind tag="count">3</bind></apply>
  <apply template="creature_row"><bind tag="count">4</bind></apply>
</table>
</fieldset>
<input type="submit" name="save_encounter" value="Save">
<input type="submit" name="delete_encounter" value="Delete">
</form>
</div>
</div>
