package web;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

@Service
public class AuthServ {
	@Autowired
	private AuthRepo authRepo;
//	@Autowired 
//	private AuthorityRepository authRepo;

	public void add(String user) {
		Auth n = new Auth();
		n.setName(user);
		n.setAuth("ROLE_USER");
		authRepo.save(n);
	}
}