package web;

import javax.persistence.*;

@Entity // This tells Hibernate to make a table out of this class
@Table(name = "vulns")
public class Vulns {
    @Id
    @GeneratedValue(strategy=GenerationType.AUTO)
    private Integer id;

    private String vuln;

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getVuln() {
        return vuln;
    }

    public void setVuln(String vuln) {
        this.vuln = vuln;
    }


}