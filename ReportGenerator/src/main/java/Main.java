
import org.docx4j.dml.wordprocessingDrawing.Inline;
import org.docx4j.jaxb.Context;
import org.docx4j.model.table.TblFactory;
import org.docx4j.openpackaging.exceptions.Docx4JException;
import org.docx4j.openpackaging.exceptions.InvalidFormatException;
import org.docx4j.openpackaging.packages.WordprocessingMLPackage;
import org.docx4j.openpackaging.parts.WordprocessingML.BinaryPartAbstractImage;
import org.docx4j.openpackaging.parts.WordprocessingML.MainDocumentPart;
import org.docx4j.wml.*;

import java.io.*;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class Main {

    public static void main(String[] args) {
        ArrayList<VulnModel> listVuln = new ArrayList<VulnModel>();
        System.out.println("Step 1: Get data from mysql database");
        try {
            Class.forName("com.mysql.jdbc.Driver");
            Connection conn = DriverManager.getConnection("jdbc:mysql://localhost:3306/vuln", "mimi", "mimi123");
            Statement stmt = conn.createStatement();

            System.out.println(" Select all Description");
            ResultSet rs = stmt.executeQuery("SELECT * FROM vuln limit 5;");
            while (rs.next()) {
                listVuln.add(new VulnModel(rs.getInt(1), rs.getString(2), rs.getString(3)));
            }

            rs.close();
            stmt.close();
        } catch (ClassNotFoundException cE) {
            cE.printStackTrace();
        } catch (SQLException ex) {
            ex.printStackTrace();
        }

        convertToPdfAndHtml("inputfile/last.png", listVuln);
    }

    public static void convertToPdfAndHtml(String imagePath, ArrayList<VulnModel> fiveVulns) {
        Converter converter = new Converter();
        String intro = "Congratulation! You got your report generated and that is an explaination graph for you";
        String lastSession = "Instructions to how to use the virtual machine";
        try {
            System.out.println("Step 2 : Write data to docx file");
            writeDataToDoc(lastSession, intro, imagePath, fiveVulns);
            System.out.println("Step 3 : write data to html file");
            converter.executeHtml("inputfile/test.docx", "outputfiles/output.html");
            System.out.println("Step 4 : Write data to pdf file");
            converter.executePdf("inputfile/test.docx", "outputfiles/output.pdf");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    public static void writeDataToDoc(String lastSession, String intros, String imageInput, ArrayList<VulnModel> vulnModels) {
        try {
            WordprocessingMLPackage wordPackage = WordprocessingMLPackage.createPackage();
            MainDocumentPart mainDocumentPart = wordPackage.getMainDocumentPart();
            // title
            mainDocumentPart.addParagraphOfText(intros);
            // image
            File image = new File(imageInput);
            byte[] fileContent = read(image);
            BinaryPartAbstractImage imagePart = BinaryPartAbstractImage
                    .createImagePart(wordPackage, fileContent);
            Inline inline = imagePart.createImageInline(
                    "Baeldung Image (filename hint)", "Alt Text", 1, 2, false);
            P Imageparagraph = addImageToParagraph(inline);
            mainDocumentPart.getContent().add(Imageparagraph);

            // table
            int writableWidthTwips = wordPackage.getDocumentModel()
                    .getSections().get(0).getPageDimensions().getWritableWidthTwips();
            int columnNumber = 3;
            Tbl tbl = TblFactory.createTable(5, 3, writableWidthTwips/columnNumber);
            List<Object> rows = tbl.getContent();
            ObjectFactory factory = Context.getWmlObjectFactory();
            int index = 0;
            for (Object row : rows) {
                Tr tr = (Tr) row;
                List<Object> cells = tr.getContent();
                VulnModel vulnModel = vulnModels.get(index);
                int count = 0;
                for(Object cell : cells) {
                    Tc td = (Tc) cell;
                    // create p
                    P p = factory.createP();
                    R r = factory.createR();
                    Text t = factory.createText();
                    if (count == 0)
                        t.setValue(String.valueOf(vulnModel.getVulnId()));
                    else if (count == 1)
                        t.setValue(vulnModel.getVulnName());
                    else
                        t.setValue(vulnModel.getVulnDescription());
                    r.getContent().add(t);
                    p.getContent().add(r);
                    td.getContent().add(p);
                    count++;
                }

                index++;
            }
            mainDocumentPart.addObject(tbl);

            // add lastsession
            mainDocumentPart.addParagraphOfText(lastSession);
            // export file
            File exportFile = new File("inputfile/test.docx");
            wordPackage.save(exportFile);
        } catch (InvalidFormatException ifo) {
            ifo.printStackTrace();
        } catch (Docx4JException d4e) {
            d4e.printStackTrace();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    public static byte[] read(File file) throws IOException {
        ByteArrayOutputStream ous = null;
        InputStream ios = null;
        try {
            byte[] buffer = new byte[4096];
            ous = new ByteArrayOutputStream();
            ios = new FileInputStream(file);
            int read = 0;
            while ((read = ios.read(buffer)) != -1) {
                ous.write(buffer, 0, read);
            }
        }finally {
            try {
                if (ous != null)
                    ous.close();
            } catch (IOException e) {
            }

            try {
                if (ios != null)
                    ios.close();
            } catch (IOException e) {
            }
        }
        return ous.toByteArray();
    }

    private static P addImageToParagraph(Inline inline) {
        ObjectFactory factory = new ObjectFactory();
        P p = factory.createP();
        R r = factory.createR();
        p.getContent().add(r);
        Drawing drawing = factory.createDrawing();
        r.getContent().add(drawing);
        drawing.getAnchorOrInline().add(inline);
        return p;
    }
}
