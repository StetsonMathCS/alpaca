# JDBC Setup for Alpaca

## Requisite software :

+ Java 8
+ jConnector
+ mySQL

## Installing

Installing jConnector to the Java project to access the database
Here is the download link for jConnector
https://dev.mysql.com/downloads/connector/j/

### Add to the build path

In order to connect mySQL to the java code, the jConnector should be added in the JAR library under build path 


### Connects to the server:

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

it counts the vulnerabilities and execute a query from Java in order to count them

### Example:
<pre>
System.out.println("3. Which vulnerabilities employ a brute force technique to login?");
</pre>

Here, it should take a sql query the %like% and display any vulnerability that has brute force

## Run the Program

In order to run the program, the user must login to the ssh server
using the tunnel in the command line

<pre>
$ssh username@remoteserver -L 3306:localhost:3306 
</pre>
### * Note : the mySQL server should be stopped in your local machine before executing the program. Other wise, the address would be used twice and might cause an error.

