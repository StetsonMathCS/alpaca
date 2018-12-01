package web;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

@Service
public class MachineService {
	@Autowired
	private MachineRepo UserRepository;

	public void add(String user, String privacy, String owner) {
		Machine n = new Machine();
		n.setName(user);
		n.setPrivacy(privacy);
		n.setOwner(owner);
		UserRepository.save(n);
		
	}
}
