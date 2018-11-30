public enum  OutputFormat {
    HTML("html"),
    PDF("pdf");

    private String formatName;
    private OutputFormat(String name) {
        this.formatName = name;
    }

    public String getFormatName() {
        return this.formatName;
    }
}
