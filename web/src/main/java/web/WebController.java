package web;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.StringWriter;
import java.security.Principal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import javax.imageio.ImageIO;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.web.servlet.error.ErrorController;
import org.springframework.core.io.InputStreamResource;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;

import com.google.code.kaptcha.impl.DefaultKaptcha;
import com.mashape.unirest.http.Unirest;
import com.mashape.unirest.http.exceptions.UnirestException;
import com.mashape.unirest.request.HttpRequestWithBody;

@Controller
public class WebController implements ErrorController {

	@Autowired
	private UserService UserService;
	@Autowired
	private AuthServ authService;
	@Autowired
	private MachineService machService;
	@Autowired
	UserRepository userRepository;
	@Autowired
	VulnsRepository vulnsRepository;
	@Autowired
	MachineRepo machineRepo;
	@Autowired
	DefaultKaptcha defaultKaptcha;

	private String captcha;

	@GetMapping("/home")
	public ModelAndView home() {
		ModelAndView model = new ModelAndView("index");
		model.addObject("msg", vulnsRepository.count());
		return model;
	}

	@GetMapping("/error")
	public ModelAndView error() {
		ModelAndView model = new ModelAndView("index");
		model.addObject("msg", vulnsRepository.count());
		return model;
	}

	@Override
	public String getErrorPath() {
		return "index";
	}

	@GetMapping("/builder")
	public ModelAndView builder() {
		ModelAndView model = new ModelAndView("create");
		model.addObject("msg", listOfVulns());
		return model;
	}

	@GetMapping("/page1")
	public ModelAndView page(Principal principal) throws UnirestException, JSONException {
		ModelAndView model = new ModelAndView("page1");

		int i = 0;
		int j = 0;
		List<String> list = new ArrayList();
		while (i < machineRepo.count()) {
			if (machineRepo.findById((j)).isPresent()) {
				if (machineRepo.findById(j).get().getOwner().compareTo(principal.getName()) == 0) {
					list.add(machineRepo.findById((j)).get().getName());
				}
				i++;
			}
			j++;
		}
		model.addObject("msg", list.toString());
		return model;
	}

	@GetMapping("/page2")
	public ModelAndView showForm(Principal principal) {
		ModelAndView model = new ModelAndView("page2");

		int i = 0;
		int j = 0;
		List<String> list = new ArrayList();
		while (i < machineRepo.count()) {
			if (machineRepo.findById((j)).isPresent()) {
				if (machineRepo.findById(j).get().getPrivacy().compareTo("PUBLIC")==0) {
					list.add(machineRepo.findById((j)).get().getName());
				}
				i++;
			}
			j++;
		}
		model.addObject("msg",list.toString());
		return model;
	}

	@PostMapping(value = "/postData")
	public ModelAndView submitAdmissionForm(@RequestParam("test") String t) {
		ModelAndView model = new ModelAndView("postData");
		model.addObject("msg", "POST DATA: " + t);
		return model;
	}

	@PostMapping(value = "/postBuild")
	public ModelAndView submitBuilder(@RequestParam("name") String name, @RequestParam("goal") String goal,
			@RequestParam("privacy") String privacy, Principal principal) throws JSONException, UnirestException {
		boolean check = StringUtils.containsAny(name, "!@#$%^&*()-=+{}[]|\":;''<>,.?/");
	//	boolean check2 = StringUtils.containsAny(goal, "!@#$%^&*()-=+{}[]|\":;''<>.?/");
	//	boolean check3 = StringUtils.containsAny(name, "!@#$%^&*()-=+{}[]|\":;''<>,.?/");
		ModelAndView model = new ModelAndView("index");
	
		//model.addObject("msg", "POST DATA: " + goal);
		HttpRequestWithBody alpacaReq = Unirest.post("http://127.0.0.1:10333/alpaca")
				.header("content-type", "application/json; charset=utf-8").header("accept", "application/json");
		StringWriter reqBodyWriter = new StringWriter();
		JSONWriter reqBodyJSONWriter = new JSONWriter(reqBodyWriter).array();
		reqBodyJSONWriter.value("createRangeFromIGS");
		reqBodyJSONWriter.value("[" + goal + "]");
		reqBodyJSONWriter.value("[]");
		reqBodyJSONWriter.value(name);
		reqBodyJSONWriter.endArray();
		System.out.println(reqBodyWriter.toString());
		JSONArray alpaca_resp = alpacaReq.body(reqBodyWriter.toString()).asJson().getBody().getArray();
		System.out.print(alpaca_resp);
		machService.add(name + "1", privacy, principal.getName());
		
		return model;
	}

	@GetMapping(value = "/vulns")
	public ModelAndView vulnsList() {
		ModelAndView model = new ModelAndView("vulnList");
		String vuln = listOfVulns().toString();
		model.addObject("msg", vuln);
		return model;
	}

	@PostMapping(value = "/postLog")
	public ModelAndView submitLogin(@RequestParam("user") String user, @RequestParam("pass") String pass,
			@RequestParam("tryCode") String tCap) {
		ModelAndView model = new ModelAndView("postData");

		String u = user;
		String p = pass;
		boolean check = StringUtils.containsAny(u, "!@#$%^&*()-=+{}[]|\":;''<>,.?/");
		
		
		if (tCap.compareTo(captcha) == 0 && !listOfUsers().contains(u) && !check) {
			UserService.add(u, p);
			authService.add(u);
			model.addObject("msg", "USER: " + user + " PASS: " + pass);
		} else {
			model.addObject("msg", "Failed");
		}

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

//	@GetMapping("/all")
//	public @ResponseBody Optional<User> getAllUsers() {
//
//		return userRepository.findById(1);
//	}

	@RequestMapping("/Register")
	public void defaultKaptcha(HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse)
			throws Exception {
		byte[] captchaChallengeAsJpeg = null;
		ByteArrayOutputStream jpegOutputStream = new ByteArrayOutputStream();
		try {
			String createText = defaultKaptcha.createText();
			captcha = createText;
			httpServletRequest.getSession().setAttribute("rightCode", createText);
			BufferedImage challenge = defaultKaptcha.createImage(createText);
			ImageIO.write(challenge, "jpg", jpegOutputStream);
		} catch (IllegalArgumentException e) {
			httpServletResponse.sendError(HttpServletResponse.SC_NOT_FOUND);
			return;
		}

		captchaChallengeAsJpeg = jpegOutputStream.toByteArray();
		httpServletResponse.setHeader("Cache-Control", "no-store");
		httpServletResponse.setHeader("Pragma", "no-cache");
		httpServletResponse.setDateHeader("Expires", 0);
		httpServletResponse.setContentType("image/jpeg");
		ServletOutputStream responseOutputStream = httpServletResponse.getOutputStream();
		responseOutputStream.write(captchaChallengeAsJpeg);
		responseOutputStream.flush();
		responseOutputStream.close();
	}
	
	@GetMapping("/logout2")
	public String logout(HttpServletRequest request, HttpServletResponse response) {
		HttpSession session = request.getSession(false);
		SecurityContextHolder.clearContext();
		session=request.getSession(false);
		if(session !=null) {
			session.invalidate();
		}
		for(Cookie cookie:request.getCookies()) {
			cookie.setMaxAge(0);
		}
		return "index";
	}

	private List<String> listOfVulns() {
		int i = 0;
		int j = 0;
		List<String> list = new ArrayList();
		while (i < vulnsRepository.count()) {
			if (vulnsRepository.findById((j)).isPresent()) {
				list.add(vulnsRepository.findById((j)).get().getVuln());
				i++;
			}
			j++;
		}
		return list;
	}
	
	private List<String> listOfUsers() {
		int i = 0;
		int j = 0;
		List<String> list = new ArrayList();
		while (i < userRepository.count()) {
			if (userRepository.findById((j)).isPresent()) {
				list.add(userRepository.findById((j)).get().getName());
				i++;
			}
			j++;
		}
		return list;
	}

}