package test;
//package web;
//
//import org.junit.Before;
//import org.junit.Test;
//import org.springframework.beans.factory.annotation.Autowired;
//
//import static org.junit.Assert.assertTrue;
//
//public class UserTest {
//
//    @Autowired
//    private UserRepository repository;
//    private UserService service;
//    User user;
//
//    @Before
//    public void setUp(){
//        user = new User();
//        user.setId(1);
//        user.setName("new");
//        user.setPassword("passcode");
//        user.setTrue();
//    }
//
//    @Test
//    public void getId(){
//        assertTrue(user.getId() == 1);
//    }
//
//    @Test
//    public void setId(){
//        user.setId(4);
//        assertTrue(user.getId() == 4);
//    }
//
//    @Test
//    public void getName(){
//        assertTrue(user.getName() =="new");
//    }
//
//    @Test
//    public void setName(){
//        user.setName("info");
//        assertTrue(user.getName() == "info");
//    }
//
//    @Test
//    public void getPassword() {
//        assertTrue(user.getPassword() =="passcode");
//    }
//
//    @Test
//    public void setPassword() {
//        user.setPassword("secret");
//        assertTrue(user.getPassword() == "secret");
//    }
//
//    @Test
//    public void checkTrue() {
//        assertTrue(user.enabled == true);
//    }
//}