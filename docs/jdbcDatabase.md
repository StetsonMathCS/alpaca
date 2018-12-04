# JDBC Setup for Alpaca

## Requisite software :

+ Java 8
+ jConnector

## Installing

Installing jConnector to the Java project to access the database

### Example:

<pre>
Connection connection = DriverManager.getConnection("path/to/the/database", username, password);
//username is the database username, and the password of that username
</pre>

## 12 Features:

In the project, it has 12 features that has been executed, 
the idea from them is to have easy access and manuiplaute database queires
in the java code.

### Example:
<pre>
System.out.println("1. What is the total number of vulnerabilities?");
</pre>

it count the vulnerabilities and execute a query from Java in order to count them

### Example:
<pre>
System.out.println("3. Which vulnerabilities employ a brute force technique to login?");
</pre>

Here, it should take a sql query the %like% and display any vulnerability that has brute force

## Run the Program

In order to run the program, the user must have login to the ssh server
using the tunnel in the command line

<pre>
$ssh username@remoteserver -L 3306:localhost:3306 
</pre>
* Note : the mySQL server should be stopped in your local machine before executing the program. Other wise, the address would be used twice and might cause an error.

