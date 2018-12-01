package web;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.net.URI;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.security.Principal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.imageio.ImageIO;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.web.servlet.error.ErrorController;
import org.springframework.core.io.InputStreamResource;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

import com.google.code.kaptcha.impl.DefaultKaptcha;
import com.mashape.unirest.http.JsonNode;
import com.mashape.unirest.http.Unirest;
import com.mashape.unirest.http.exceptions.UnirestException;
import com.mashape.unirest.request.HttpRequestWithBody;

@RestController
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
		model.addObject("msg", list);
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
				if (machineRepo.findById(j).get().getPrivacy().compareTo("PUBLIC") == 0) {
					list.add(machineRepo.findById((j)).get().getName());
				}
				i++;
			}
			j++;
		}
		model.addObject("msg", list);
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
			@RequestParam("privacy") String privacy, Principal principal, @RequestParam("goal") String goal2)
			throws JSONException, UnirestException, IOException {
		boolean check = StringUtils.containsAny(name, "!@#$%^&*()-=+{}[]|\":;''<>,.?/");
		boolean check2 = StringUtils.containsAny(goal, "!@#$%^&*()-=+{}[]|\":;''<>.?/");
		boolean check3 = StringUtils.containsAny(privacy, "!@#$%^&*()-=+{}[]|\":;''<>,.?/");
		boolean check4 = StringUtils.containsAny(goal2, "!@#$%^&*()-=+{}[]|\":;''<>.?/");
		ModelAndView model = new ModelAndView("index");

		// model.addObject("msg", "POST DATA: " + goal);
		if (!check && !check2 && !check3 && !check4
				&& ((privacy.compareTo("PUBLIC") == 0) || privacy.compareTo("PRIVATE") == 0)) {
			HttpRequestWithBody alpacaReq = Unirest.post("http://127.0.0.1:10332/alpaca")
					.header("content-type", "application/json; charset=utf-8").header("accept", "application/json");
			StringWriter reqBodyWriter = new StringWriter();
			JSONWriter reqBodyJSONWriter = new JSONWriter(reqBodyWriter).array();
			reqBodyJSONWriter.value("createRangeFromIGS");
			reqBodyJSONWriter.value("[" + goal + "]");
			reqBodyJSONWriter.value("[" + goal2 + "]");
			reqBodyJSONWriter.value(name);
			reqBodyJSONWriter.endArray();
			System.out.println(reqBodyWriter.toString());
			JsonNode alpaca_resp = alpacaReq.body(reqBodyWriter.toString()).asJson().getBody(); // .getArray();
			System.out.print(alpaca_resp);
			machService.add(name + "1", privacy, principal.getName());
			zip2(name + "1");
		}

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

	@GetMapping("/register")
	public ModelAndView showRegister() {
		ModelAndView model = new ModelAndView("register");
		return model;
	}

	// using this page as reference
	// https://www.oodlestechnologies.com/blogs/How-To-Download-A-File-Directly-From-URL-In-Spring-Boot
	@GetMapping(value = "/download")
	public InputStreamResource FileSystemResource2(@RequestParam("name") String name, HttpServletResponse resp,
			Principal principal) throws IOException {

		boolean check = StringUtils.containsAny(name, "!@#$%^&*()-=+{}[]|\":;''<>,.?/");

		if (!check) {
			int id = getID(name, principal.getName());

			if (machineRepo.findById(id).get().getOwner().compareTo(principal.getName()) == 0
					|| machineRepo.findById(id).get().getPrivacy().compareTo("PUBLIC") == 0) {
				resp.setContentType("application/zip");
				resp.setHeader("Content-Disposition",
						"attachment; filename=" + "\" " + machineRepo.findById(id).get().getName() + ".zip" + "\"");
				InputStreamResource res = new InputStreamResource(new FileInputStream(
						"/home/greg/oldApi/" + machineRepo.findById(id).get().getName() + ".zip"));
				System.out.println(name);
				return res;
			} else {
				return null;
			}
		}
		return null;
	}

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

	private List<String> listOfVulns() {
		int i = 0;
		int j = 0;
		List<String> list = new ArrayList<String>();
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
		List<String> list = new ArrayList<String>();
		while (i < userRepository.count()) {
			if (userRepository.findById((j)).isPresent()) {
				list.add(userRepository.findById((j)).get().getName());
				i++;
			}
			j++;
		}
		return list;
	}

	// using
	// https://docs.oracle.com/javase/7/docs/technotes/guides/io/fsp/zipfilesystemprovider.html

	public void zip2(String machine) throws IOException {
		Map<String, String> env = new HashMap<>();
		env.put("create", "true");
		String mach = machine;
		URI uri = URI.create("jar:file:/home/greg/oldApi/" + machine + ".zip");
		String[] exfiles = new String[3];
		exfiles[0] = "/lattice.gv.png";
		exfiles[1] = "/lattice.gv";
		exfiles[2] = "/vars/all.yml";

		String[] infiles = new String[3];
		infiles[0] = "/lattice.gv.png";
		infiles[1] = "/lattice.gv";
		infiles[2] = "/all.yml";

		for (int i = 0; i < exfiles.length; i++) {
			try (FileSystem zipfs = FileSystems.newFileSystem(uri, env)) {
				Path exFile = Paths.get("/home/greg/oldApi/" + mach + exfiles[i]);
				Path inFile = zipfs.getPath(infiles[i]);
				Files.copy(exFile, inFile, StandardCopyOption.REPLACE_EXISTING);
			}
		}
	}

	public int getID(String machine, String name) {
		int i = 0;
		int j = 0;
		while (i < machineRepo.count()) {
			if (machineRepo.findById((j)).isPresent()) {
				if (machineRepo.findById(j).get().getName().compareTo(machine) == 0) {
					return j;
				}
				System.out.println(machineRepo.count());
				i++;
			}
			j++;
		}
		return i;
	}
}