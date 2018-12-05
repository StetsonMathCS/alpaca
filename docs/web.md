# Requirements
* Spring
* Maven
* Java

# Configuring the website
The website relies on information gathered from a vulnerability database. Check that the website is able to connect to your database by referencing the applicaiton.properties file. Adjust spring.datasource.username and spring.datasource.password to match your mysql username and password. Also make sure the correct database is being used for spring.datasource.url. Check that the alpacaPath variable is correctly set in alpaca/web/java/web/WebController.java. The variable should match the location of the repository.

# Running the website
The website can be ran by navigating to the web folder and running "mvn spring-boot:run". By default, the website will be running on port 8080. Maven is required.

# Creating an account
Many of the features available on the website require a user to be logged in. Users can create an account at "/register". After an account is created, navigate back to the home page at /home.

# Creating a machine
Creating a machine requires you to be logged in. For creating an account, see "Creating an account." The UI for creating a machine can be found by selecting "Cyber Range" on the navbar of the home page or navigating to /builder. From there you can set your machine's initial and end goals. You are also able to name your machine, and decide if you want it to be publicly available for download. 

# Downloading your machine
Privately created machines, and machines the logged in user has created, can be downloaded at /page1. Public machines can be found at /page2 . A user not logged in can view public machines, but not download them.
