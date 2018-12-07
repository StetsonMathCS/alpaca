# Requirements
* Maven
* Java
* Tomcat (Windows)

# Configuring the website
The website relies on information gathered from a vulnerability database. Check that the website is able to connect to your database by referencing the applicaiton.properties file.   
  
> spring.jpa.show-sql = true  

> spring.jpa.hibernate.ddl-auto=update  

> spring.datasource.url=jdbc:mysql://localhost:3306/'database name'  

> spring.datasource.username='user name'  

> spring.datasource.password='password'  
  
  
Adjust spring.datasource.username and spring.datasource.password to match your mysql username and password. Also make sure the correct database is being used for spring.datasource.url. Check that the alpacaPath variable is correctly set in alpaca/web/java/web/WebController.java. The variable should match the location of the repository.

# Package the program for IntelliJ IDEA user
In the Run/Debug Configurations, add a new Configuration choosing Maven. Write 'clean package' in the Comand line under Parameters. The Software will automatically compile the Project and pack it into war/jar file. The package will stored under target folder.

# Tomcat setting
Because our web ultimately needs to run on sever, we use Tomcat. Spring boots embedded within Tomcat. When using an external Tomcat server, must exclude embedded Tomcat servers then add Tomcat dependency.

# Running the website
## Windows environment
- First need using Putty connect to the sever
- To run the program, user need install Tomcat 8.5.34 (the version that we used) 
- Get the war file under target folder
- Put war file into tomcat->webapps
- Go to tomcat->bin
- Run the Project command:  ./catalina.sh run

## Linux environment
The website can be ran by navigating to the web folder and running "mvn spring-boot:run". By default, the website will be running on port 8080. Maven is required.

# Creating an account
Many of the features available on the website require a user to be logged in. Users can create an account at "/register". On this page user have to input user name and password. Also the user requied to input the right captcha code shown on the page. After an account is created, navigate back to the home page at /home.

# Creating a machine
Creating a machine requires you to be logged in. For creating an account, see "Creating an account." The UI for creating a machine can be found by selecting "Cyber Range" on the navbar of the home page or navigating to /builder. From there you can set your machine's initial and end goals. You are also able to name your machine, and decide if you want it to be publicly available for download. 

# Downloading your machine
Privately created machines, and machines the logged in user has created, can be downloaded at /page1. Public machines can be found at /page2 . A user not logged in can view public machines, but not download them.

# Testcase
We have couple testcase to test the entity (used for connecting database) and controller. The testcases partly work. When we try test on server, it crashed. We found out the issue occurred because of Pom file. We could not solve it. 
 
