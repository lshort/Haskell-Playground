 <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" 
"http://www.w3.org/TR/html4/loose.dtd">
<html>
<head><title>Cheeses</title></head>

<body>
<form method="get" name="nameform">
<uri_text_w_save  name="name" value="uri:name" label="Name To Edit/Create"/>
<input type="submit" value="Edit/Create">
</form>
<br />
<form method="post" name="dataform" action="cheese_template?_task=save">
<uri_text_input name="name" value="uri:name" label="Name"/>
<sql_text_input table="cheese" key="name" value="uri:name" column="country" label="Country"/>
<sql_text_input table="cheese" key="name" value="uri:name" column="price" label="Price"/>
<sql_text_input table="cheese" key="name" value="uri:name" column="stock" label="Stock"/>
<input type="submit" value="Save">
</form>
<br />





<br />  <br />
<apply template="nav">
</body>
</html>
