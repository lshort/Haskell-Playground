<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" 
"http://www.w3.org/TR/html4/loose.dtd">
<html>
<head><title>Cheeses</title></head>

<body>
<get_sql_where_attr column="price" key="name" value="'dubliner'" table="cheese">
</get_sql_where_attr>
<br />
<get_sql_where>
  <column>country</column>
  <table>cheese</table>
  <where_clause>name &lt;&gt; 'dubliner'</where_clause>
</get_sql_where>
<br />
Template 
<br />
<get_sql_basic>
  <column>name</column>
  <table>cheese</table>
</get_sql_basic>
<br />
<get_sql_basic>
  <column>*</column>
  <table>cheese</table>
</get_sql_basic>
<br />  <br />
<apply template="nav"/>
</body>
</html>
