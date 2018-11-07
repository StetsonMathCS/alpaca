package web;

import javax.persistence.*;

@Entity // This tells Hibernate to make a table out of this class
@Table(name = "authorities")
public class Auth {
    @Id
    @GeneratedValue(strategy=GenerationType.AUTO)
    private Integer id;

    private String username;

    private String authority;

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getName() {
        return username;
    }

    public void setName(String name) {
        this.username = name;
    }

    public String getAuth() {
        return authority;
    }

    public void setAuth(String auth) {
        this.authority = auth;
    }
    


}