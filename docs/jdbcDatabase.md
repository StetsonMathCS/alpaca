# JDBC Setup for Alpaca

## Requisite software :

+ Java 8
+ jConnector
+ mySQL
+ Solr 

##Installing MySQL shell on Linux

<pre>
sudo apt-get update 

apt-get install mysql-server
</pre>

##Getting into the MySQL shell  

<pre>
mysql -u root
</pre>

##Setting up a superuser

<pre>
CREATE USER 'username'@'localhost' IDENTIFIED BY 'password';
</pre>

##Grant all privileges to that user so it can have root privileges 

<pre>
GRANT ALL PRIVILEGES ON *.* TO 'user'@'localhos';
</pre>

## Getting into the database 
First ssh into your server. Then use your username and your password to get into MySQL. 

<pre>
mysql -u username -p password 
</pre>

## Get into database 

<pre>
use alpaca;
</pre>

## see all tables

<pre>
show tables;
</pre>

## backup database 
By default, mysqldump writes information as SQL statements to the standard output. Save the output in a file: 

<pre>
shell> mysqldump --user=username --password --lock-tables --databases db_name > db_name.sql
</pre>

## Restoring database from mysqldump

<pre>
mysql -u[username] -p[password] -h[hostname] [database name] < [filename].sql
</pre>

## Installing

Installing jConnector to the Java project to access the database
Here is the download link for jConnector
https://dev.mysql.com/downloads/connector/j/

### Add to the build path

In order to connect mySQL to the java code, the jConnector should be added in the JAR library under build path 

<pre>
$ wget [url with the file needed]
</pre>

Copy file from the downloaded archive 'mysql-connector-java-*.jar' to the folder 'contrib/dataimporthandler/lib' in the folder where Solr is installed. You may need to create the lib folder if you don't have it.

<pre>
$ cp mysql-connector-java-5.1.30-bin.jar ../Solr/solr-7.5.0/contrib/dataimporthandler/lib 
</pre>

Your desired directory will have a slightly different path. The above cp command is ran while I was in the .jar directory

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
### Execute:
<pre>
ResultSet rs = statement.executeQuery("SELECT COUNT(*) FROM vuln;");
            //get the resulting number
            rs.next();
            int count = rs.getInt(1);
            //print out the answer
            System.out.println("\tThere is a total of " + count + " vulnerabilities.");
</pre>
it counts the vulnerabilities and execute a query from Java in order to count them

### Example:
<pre>
System.out.println("3. Which vulnerabilities employ a brute force technique to login?");
</pre>
### Execute:
<pre>
rs = statement.executeQuery("SELECT vuln_id, vuln_name, vuln_description FROM vuln WHERE vuln_name LIKE '%brute-force%';");
            //zeros the counter
            int i = 0;
            while (rs.next()) {
                //increase the counter
                i++;
                //print out the answer
                System.out.println("\tID = " + rs.getInt(1) + ", Vulnerability: " + rs.getString(2) + ", Description: " + rs.getString(3));
            }

</pre>
Here, it should take a sql query the %like% and display any vulnerability that has brute force

## Run the Program

In order to run the program, the user must login to the ssh server
using the tunnel in the command line

<pre>
$ssh username@remoteserver -L 3306:localhost:3306 
</pre>
### * Note : the mySQL server should be stopped in your local machine before executing the program. Other wise, the address would be used twice and might cause an error.
![alt tunnel](./img/tunnel.jpg)

# Solr 
Apache Solr is an open source enterprise search engine for data stored in HDFS which enables organizations to perform full-text search and real-time indexing. Many applications store data in Relational databases, and searching is a common use case that you'll have to deal with. You can search the data in Relational or structured databases by importing the data using Apache Solr's Data Import Handler. This is a way of importing data from a database using the JBDC drivers and indexing it.

## Setting up Solr using Putty 

### Download and install Solr 
Prereq: You will need the Java Runtime Environment (JRE) verion 1.8 or higher. You can check your Java version like this:

<pre>
$ java -version
Java(TM) SE Runtime Environment (build 1.8.9_60-b27)
Java HotSpot(TM) 64-Bit Server VM (build 25.60-b23, mixed mode)
</pre>

Download and install Solr from https://lucene.apache.org/solr/ You can use curl or wget.

<pre>
$ wget [url/file.tar]
</pre>

Make sure to extract the contents of the downloaded folder. There are many references on Google how to do that depending on what file extension you have downloaded.

### Setup a new collection
bin/solr create -c products This will create a new collection named products which will be visible from Solr when you try to see what collections you have.

### solrconfig.xml
edit solrconfig.xml (/solr/example/files/conf) by adding:

```html
<requestHandler name="/dataimport" class="org.apache.solr.handler.dataimport.DataImportHandler">
	<lst name="default">
		<str name="config"data-config.xml</str>
	</lst>
</requestHandler>
```

## data-config.xml for MYSQL database
You can find a file, if you don't know where it is, with the command:

<pre>
$ locate filename.ext
</pre>

The file 'data-config.xml' will define data we want to import/index from our datasource.

### Start Solr 
prereq: Setup Putty in Tunnels in SSH to allow localhost to link to your desired port.

Change into your Solr directory and run:

<pre>
$ bin/solr start -e cloud 
</pre>

This will give you some options to configure Solr. Then you can go to your browser and open localhost:8983 The port can be any available port. It does not have to be 8983

### References 
* https://www.progress.com/tutorials/jdbc/import-sql-server-data-into-apache-solr-using-data-import-handler-via-jdbc

* https://gist.github.com/rnjailamba/8c872768b136a88a10b1

