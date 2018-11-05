package web;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Optional;

import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.InputStreamResource;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;

import web.UserRepository;
import web.VulnsRepository;

@Controller
public class MilestoneController {

	@Autowired
	private UserService UserService;
	//@Autowired
	//private VulnsService vulnsService;
	@Autowired
	UserRepository userRepository;
	@Autowired
	VulnsRepository vulnsRepository;

	@GetMapping("/home")
	public String home() {
		return "index";
	}

	@GetMapping("/builder")
	public String builder() {
		return "create";
	}

	@GetMapping("/page1")
	public String page() {
		return "page1";
	}

	@GetMapping("/page2")
	public ModelAndView showForm() {
		ModelAndView model = new ModelAndView("page2");
		return model;
	}

	@PostMapping(value = "/postData")
	public ModelAndView submitAdmissionForm(@RequestParam("test") String t) {
		ModelAndView model = new ModelAndView("postData");
		model.addObject("msg", "POST DATA: " + t);
		return model;
	}

	@PostMapping(value = "/postBuild")
	public ModelAndView submitBuilder(@RequestParam("vuln") String v1) {
		ModelAndView model = new ModelAndView("postData");
		model.addObject("msg", "POST DATA: " + v1);
		return model;
	}

	@GetMapping(value = "/vulns")
	public ModelAndView vulnsList() {
		ModelAndView model = new ModelAndView("vulnList");
		String vuln = "List: \n";
		int i = 0;
		int j = 0;
		while (i < vulnsRepository.count()) {
			if (vulnsRepository.findById((j)).isPresent()) {
				vuln += vulnsRepository.findById((j)).get().getVuln() + "("
						+ vulnsRepository.findById((j)).get().getId() + ")\n";
				i++;
			}
			j++;
		}
		model.addObject("msg", vuln);
		return model;
	}

	@PostMapping(value = "/postLog")
	public ModelAndView submitLogin(@RequestParam("user") String user, @RequestParam("pass") String pass) {
		ModelAndView model = new ModelAndView("postData");
		model.addObject("msg", "USER: " + user + " PASS: " + pass);

		String u = user;
		String p = pass;
		UserService.add(u, p);

		return model;
	}

	@GetMapping("/login")
	public ModelAndView showLogin() {
		ModelAndView model = new ModelAndView("login");
		return model;
	}
	
	@GetMapping("/register")
	public ModelAndView showRegister() {
		ModelAndView model = new ModelAndView("register");
		return model;
	}

	// using this page as reference
	// https://www.oodlestechnologies.com/blogs/How-To-Download-A-File-Directly-From-URL-In-Spring-Boot
	@GetMapping(value = "/download")
	public InputStreamResource FileSystemResource(HttpServletResponse test) throws FileNotFoundException {
		test.setContentType("application/txt");
		test.setHeader("Content-Disposition", "attachment; filename=" + "test.txt");
		InputStreamResource resource = new InputStreamResource(new FileInputStream("/home/greg/test.txt"));
		return resource;
	}

	@GetMapping("/all")
	public @ResponseBody Optional<User> getAllUsers() {

		return userRepository.findById(1);
	}
}