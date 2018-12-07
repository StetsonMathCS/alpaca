## Report Generator 

### Requirements
* Install Pandoc, Miktext, Java8, jConnector, and mySQL
  *	Pandoc: https://pandoc.org/installing.html
  * jConnector: https://dev.mysql.com/downloads/connector/j/
  * mySQL: https://www.mysql.com/downloads/
  *	MikText: https://miktex.org/
  * Java8: https://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html

* Install IDE tool : Eclipse 
  * https://www.eclipse.org/downloads/

### How to compile and run the code with eclipse 
  * Open eclipse 
  * File -> import -> Maven project 
  * Go to Main.java file -> Right click -> run 

### Features:
The code has the 9 features that was on the alpaca tasks file.
It generates a PDF and HTML report in Pandoc format and it includes:
  * an introduction paragraph.
  * the graph image.
  * a description of the graph.
  * a list of vulnerability descriptions.
  * instructions for how to use the virtual machine.

### How the code works
Step1 : get vulns from mysql database 
-	The part of the code that reads data from the database and print it up on the pdf and html report is in the Main.java file from line 22 to 39
Step2: To choose/change the graph that is shown in the report:
-	You need to go to Main.java file at line 41 and change that
convertToPdfAndHtml("inputfile/last.png", listVuln);

Step3 : generate docx file 
-	This code very important that generate data as a docx file (data from database, images, introduce). The code is writeDataToDoc function in Main.java file.

Step4: convert the docx file to pdf and html file using pandoc 
  * Code for converting pdf : executePdf function in Convert.java file
  * Code for convering html: Executehtml function in Convert.java file
