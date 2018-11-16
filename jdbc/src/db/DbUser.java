package db;

import java.sql.*;
import java.util.Properties;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

/*
 * make sure you log in using $ ssh username@remoteserver -L 3306:localhost:3306
 * you need to stop your local mysql server or use different port
 * before execute the program. 
*/
public class DbUser {
	
	private static String user;
    private static String pass;
    
    public static void main(String[] args) {
    	 Properties prop = new Properties();
         InputStream input = null;
         
         try {

             input = new FileInputStream("config.properties");

             // load a properties file
             prop.load(input);

             // get the property value and print it out
             user = prop.getProperty("username");
             pass = prop.getProperty("password");

     } catch (IOException ex) {
             ex.printStackTrace();
     } finally {
             if (input != null) {
                     try {
                             input.close();
                     } catch (IOException e) {
                             e.printStackTrace();
                     }
             }
     }
     
        DbUser dbUser = new DbUser();
        dbUser.executeQueries();
    }

    private void executeQueries() {
        try {
            Class.forName("com.mysql.jdbc.Driver");
            Connection connection = DriverManager.getConnection("jdbc:mysql://localhost:3306/vuln", user, pass);
            Statement statement = connection.createStatement();

            System.out.println("1. What is the total number of vulnerabilities?");
            //query counts the number of rows in the dom_all_vuln_infomation table
            ResultSet rs = statement.executeQuery("SELECT COUNT(*) FROM vuln;");
            //get the resulting number
            rs.next();
            int count = rs.getInt(1);
            //print out the answer
            System.out.println("\tThere is a total of " + count + " vulnerabilities.");

            System.out.println("2. What is the total number of states?");
            //query selects all different areas
            rs = statement.executeQuery("SELECT COUNT(*) FROM states;");
            //get the resulting number
            rs.next();
            count = rs.getInt(1);
            //print out the answer
            System.out.println("\tThere is a total of " + count + " states.");

            System.out.println("3. Which vulnerabilities employ a brute force technique to login?");
            rs = statement.executeQuery("SELECT vuln_id, vuln_name, vuln_description FROM vuln WHERE vuln_name LIKE '%brute-force%';");
            //zeros the counter
            int i = 0;
            while (rs.next()) {
                //increase the counter
                i++;
                //print out the answer
                System.out.println("\tID = " + rs.getInt(1) + ", Vulnerability: " + rs.getString(2) + ", Area: " + rs.getString(3));
            }

            System.out.println("4. Do any vulnerabilities require open ports?");
            rs = statement.executeQuery("SELECT vuln_id, vuln_name, vuln_description FROM vuln WHERE vuln_id IN (SELECT vuln_id FROM vuln_pre WHERE states_id = (SELECT states_id FROM states WHERE states_name = 'open_ports'));");
            while (rs.next()) {
                //print out the answer
                System.out.println("\tID = " + rs.getInt(1) + ", Vulnerability: " + rs.getString(2) + ", Description: " + rs.getString(3));
            }

            System.out.println("5. Change vulnerabilities that use port_80 to use web_access (pre -> post)");
            rs = statement.executeQuery("SELECT vuln_id FROM vuln_pre WHERE states_id = 10 GROUP BY vuln_id;");
            while (rs.next()) {
                int id = rs.getInt(1);
                System.out.println("\tUpdating vulnerability with ID = " + id);
                Statement tmpStatement = connection.createStatement();
                tmpStatement.executeUpdate("DELETE FROM vuln_post WHERE vuln_id = " + id + " AND states_id IN (SELECT states_id FROM states WHERE states_name = 'port_80' OR states_name = 'web_access');");
                tmpStatement.executeUpdate("INSERT INTO vuln_post VALUES (null, " + id + ", (SELECT states_id FROM states WHERE states_name = 'web_access'));");
                tmpStatement.close();
            }

            System.out.println("6. Is there a vulnerability that scans ports?");
            rs = statement.executeQuery("SELECT COUNT(*) FROM vuln WHERE vuln_name LIKE '%scan%';");
            rs.next();
            System.out.println("\tVulnerabilities scanning ports: " + rs.getInt(1));

            System.out.println("7. Remove vulnerabilities with ID=2 from the database.");
            int deletedPre = statement.executeUpdate("DELETE FROM vuln_pre WHERE vuln_id = 2");
            int deletedPost = statement.executeUpdate("DELETE FROM vuln_post WHERE vuln_id = 2");
            int deleted = statement.executeUpdate("DELETE FROM vuln WHERE vuln_id = 2");
            System.out.println("\tVulnerabilities deleted: " + deleted + " including " + deletedPre + " pre and " + deletedPost + " post states");
            
            System.out.println("8. Delete all vuln_post records that use the port 80");
            deleted = statement.executeUpdate("DELETE FROM vuln_post WHERE states_id = (SELECT states_id FROM states WHERE states_name = 'port_80');");
            System.out.println("\tVulnerabilities deleted: " + deleted);
            
            
            
        } catch (ClassNotFoundException | SQLException e) {
            e.printStackTrace();
        }
    }
}
