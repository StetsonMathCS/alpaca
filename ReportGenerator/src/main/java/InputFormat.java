public enum InputFormat {
    TEXTFILE("textfile");

    private String formatName;

    private InputFormat(String name) {
        this.formatName = name;
    }

    public String getFormatName() {
        return formatName;
    }
}
