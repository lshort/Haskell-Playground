  <bind tag="keys">2,region_name,encounter_name</bind>
  <bind tag="values">uri:region.region_name,uri:region_encounter.encounter_name</bind>

  <tr>
    <td>
     <sql_drop_w_none name="region_encounter.race_$(count)" column="race_name" table="race" initval="sql:region_encounter,race_$(count),$(keys),$(values)"  />
    </td><td>
     <sql_drop_w_none name="region_encounter.template_$(count)" initval="sql:region_encounter,template_$(count),$(keys),$(values)" column="tpl_name" table="template" />
    </td><td>
     <sql_text_input column="number_base_$(count)" table="region_encounter" keys="2" key2="encounter_name" keyval2="uri:region_encounter.encounter_name" key1="region_name" keyval1="uri:region.region_name" size="3" />
    </td><td>
     <sql_text_input column="dice_count_$(count)" table="region_encounter" keys="2" key2="encounter_name" keyval2="uri:region_encounter.encounter_name" key1="region_name" keyval1="uri:region.region_name" size="3" />
    </td><td>
     <sql_text_input column="dice_size_$(count)" table="region_encounter" keys="2" key2="encounter_name" keyval2="uri:region_encounter.encounter_name" key1="region_name" keyval1="uri:region.region_name" size="3" />
    </td>
  </tr>
