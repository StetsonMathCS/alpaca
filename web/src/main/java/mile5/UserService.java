package mile5;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import javax.transaction.Transactional;


@Service
public class UserService {
    @Autowired
    private UserRepository UserRepository;

//    @Transactional
    public void add(String user, String pass){
        User n = new User();
        n.setName(user);
        n.setPassword(pass);
        UserRepository.save(n);
    }

//    @Transactional
//    public void insertTwo(){
//        User UserA = new User();
//        UserA.setName("First");
//        UserA.setPassword("first");
//        UserRepository.save(UserA);
//
//        User UserB = new User();
//        UserB.setName("Second");
//        UserB.setPassword("second");
//        UserRepository.save(UserB);
//    }
}
