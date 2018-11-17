package com.finaltest;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

public class FinalTest {
	public static void main(String[] args) {
		// variable area
        List<String> datas = new ArrayList<String>();
        File file = new File("txtfile.txt");
        Converter converter = new Converter();
        System.out.println("Step 1: Get data from sql server database");
        // connect to mysql and get data area
        try {
        	Class.forName("com.mysql.jdbc.Driver") ;
        	Connection conn = DriverManager.getConnection("jdbc:mysql://localhost:3306/description?useSSL=true", "root", "112299") ;
        	Statement stmt = conn.createStatement() ;
       

            System.out.println(" Select all Description");
            ResultSet rs = stmt.executeQuery("SELECT * FROM dbo.steps");
            while (rs.next()) {
                int id = rs.getInt("id");
                String name = rs.getString("description");
                System.out.println("\t (" + id + ") " + name + " ");
                datas.add("(" + id + ") " + name + " ");
            }

            rs.close();
            stmt.close();
        } catch (ClassNotFoundException cE) {
            cE.printStackTrace();
        } catch (SQLException ex) {
            ex.printStackTrace();
        }
        // after get data so write to txt file area
        try {
            System.out.println("Step 2 : Write data to txt file");
            writeTxtFile(datas, file);
            // call convert txt file to html file area using pandoc
            System.out.println("Step 3 : write data to html file");
            converter.executeHtml("txtfile.txt", "output.html");
            // call convert pdf file area using pandoc
            System.out.println("Step 4 : Write data to pdf file");
            converter.executePdf("txtfile.txt", "output.pdf");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    public static void writeTxtFile(List<String> data, File file) {
        BufferedWriter out = null;
        try {
            out = new BufferedWriter(new FileWriter(file, true));
            for (String line:data) {
                out.write(line);
                out.newLine();
            }
            out.close();
        } catch (IOException ioex) {
            ioex.printStackTrace();
        }
    }
}