package web;

import javax.persistence.*;

@Entity // This tells Hibernate to make a table out of this class
@Table(name = "states")
public class Vulns {
    @Id
    @GeneratedValue(strategy=GenerationType.AUTO)
    private Integer states_id;

    private String states_name;

    public Integer getId() {
        return states_id;
    }

    public void setId(Integer id) {
        this.states_id = id;
    }

    public String getVuln() {
        return states_name;
    }

    public void setVuln(String vuln) {
        this.states_name = vuln;
    }


}