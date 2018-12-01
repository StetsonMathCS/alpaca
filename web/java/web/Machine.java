package web;

import javax.persistence.*;

@Entity // This tells Hibernate to make a table out of this class
@Table(name = "userMachines")
public class Machine {
    @Id
    @GeneratedValue(strategy=GenerationType.AUTO)
    private Integer id;

    private String name;
    
    private String privacy;
    
    private String owner;

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }
    
    public void setPrivacy(String priv) {
        this.privacy = priv;
    }
    
    public void setOwner(String own) {
    	this.owner=own;
    }
    
    public String getOwner() {
    	return owner;
    }
    
    public String getPrivacy() {
    	return privacy;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }


}