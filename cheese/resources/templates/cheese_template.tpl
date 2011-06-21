<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" 
"http://www.w3.org/TR/html4/loose.dtd">
<html>
<head><title>Cheeses</title></head>

<body>
<form method="get" name="nameform">
<uri_text_input  name="name" value="data:name" label="Name"/>
</form>
<br />
<sql_text_input table="cheese" key="name" value="data:name" column="price" label="Price"/>
<br />



<get_sql_with_name>
  <column>country</column>
  <table>cheese</table>
</get_sql_with_name>
<br />


<br />  <br />
<apply template="nav">
</body>
</html>
