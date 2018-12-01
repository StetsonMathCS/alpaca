package web;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

@Service
public class UserService {
	@Autowired
	private UserRepository UserRepository;

	public void add(String user, String pass) {
		User n = new User();
		n.setName(user);
		n.setPassword(new BCryptPasswordEncoder().encode(pass));
		n.setTrue();
		UserRepository.save(n);
		
	}
}
