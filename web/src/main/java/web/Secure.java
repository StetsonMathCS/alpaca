package web;

import javax.sql.DataSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;

// database follows https://docs.spring.io/spring-security/site/docs/3.0.x/reference/appendix-schema.html
// configs https://docs.spring.io/autorepo/docs/spring-security/current/reference/html/jc.html

@Configuration
@EnableWebSecurity
public class Secure extends WebSecurityConfigurerAdapter{
	
	@Autowired
	private DataSource database;
	
	@Autowired
	public void configure(AuthenticationManagerBuilder auth) throws Exception{
		auth.jdbcAuthentication().dataSource(database).passwordEncoder(new BCryptPasswordEncoder());
	}
	
	@Override
	protected void configure(HttpSecurity http) throws Exception{
		http.authorizeRequests()
		.antMatchers("/home*").permitAll()
		.antMatchers("/vulns*").hasRole("USER")
		.antMatchers("/builder*").hasRole("USER")
		.antMatchers(HttpMethod.POST, "/postBuild").hasRole("USER")
		.antMatchers("/login*").permitAll()
		.antMatchers(HttpMethod.POST, "/register").permitAll()
		.antMatchers(HttpMethod.POST,"/postLog*").permitAll()
		.antMatchers(HttpMethod.GET,"/postData*").permitAll()
		.antMatchers(HttpMethod.POST,"/page2*").permitAll()
		.antMatchers(HttpMethod.GET,"/page2*").permitAll()
		.antMatchers("/").permitAll()
		.and()
		.httpBasic();
		
		http
		.httpBasic()
		.and()
		.logout().clearAuthentication(true)
		.logoutSuccessUrl("/")
		.deleteCookies("JSESSIONID")
		.invalidateHttpSession(true)
		.and();
		
		http.csrf().disable();
		
	}

}
