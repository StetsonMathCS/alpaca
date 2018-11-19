package com.finaltest;
import java.io.File;
import java.io.IOException;

public class Converter {
    // pandoc txtfile.txt -o output.html
    // pandoc -t beamer txtfile.txt -o output.pdf

    private Setting setting;
    private String inputFilePath, outputFilePath;
    private String inputFormat, outputFormat, extraOptions;

    public Converter() {
        setting = new Setting();
        setting.setPandocExec("pandoc");
        extraOptions = "";
    }

    public void executeHtml(String inputFilePath, String outputFilePath) {
        String command = "pandoc "+inputFilePath+" -o "+outputFilePath;
        int status = 0;
        try {
            System.out.println("Executing : "+command);
            status = Runtime.getRuntime().exec(command).waitFor();
        } catch (InterruptedException ioex) {
            ioex.printStackTrace();
        } catch (IOException ioex) {
            ioex.printStackTrace();
        }

        if (status != 0) {
            System.out.println("convert to HTML failed");
        } else {
            System.out.println("Convert to HTML successful");
        }
    }

    public void executePdf(String inputFilePath, String outputFilePath) {
        String command = "pandoc -t beamer "+inputFilePath+" -o "+outputFilePath;
        int status = 0;
        try {
            System.out.println("Executing : "+command);
            status = Runtime.getRuntime().exec(command).waitFor();
        } catch (InterruptedException ioex) {
            ioex.printStackTrace();
        } catch (IOException ioex) {
            ioex.printStackTrace();
        }

        if (status != 0) {
            System.out.println("convert to Pdf failed");
        } else {
            System.out.println("Convert to Pdf successful");
        }
    }
}
