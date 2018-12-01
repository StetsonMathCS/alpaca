public class VulnModel {

    private int vulnId;
    private String vulnName;
    private String vulnDescription;

    public VulnModel(int vulnId, String vulnName, String vulnDescription) {
        this.vulnId = vulnId;
        this.vulnName = vulnName;
        this.vulnDescription = vulnDescription;
    }

    public int getVulnId() {
        return vulnId;
    }

    public void setVulnId(int vulnId) {
        this.vulnId = vulnId;
    }

    public String getVulnName() {
        return vulnName;
    }

    public void setVulnName(String vulnName) {
        this.vulnName = vulnName;
    }

    public String getVulnDescription() {
        return vulnDescription;
    }

    public void setVulnDescription(String vulnDescription) {
        this.vulnDescription = vulnDescription;
    }
}
