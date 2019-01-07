%vuln( Description, [prereqs], [result], [Role-[key-(pred, [val]),...,key-(pred,[val])]] )
%note, result cannot be empty
%note, config values must be strings or predicates


% == FTP ==

vuln('scan-ftp', [], [ftp, vsftpd234],
        [vsftpd-[version-(only, ["2.3.4"])]]).
vuln('scan-ftp', [], [ftp, vsftpd303],
        [vsftpd-[version-(only, ["3.0.3"])]]).

vuln('vsftpd-backdoor', [vsftpd234], [root_shell],
        [vsftpd-[version-(only, ["2.3.4"])]]).

% == SSH ==

vuln('scan-ssh', [], [ssh, openssh76p1],
        [openssh-[version-(only, ["7.6p1"])]]).

%vuln('ssh-login-root(brute-force)', [ssh], [root_shell],
%        [openssh-[allowrootlogin-(only, ["Yes"])],
%        users-[root-(only, [generatePassword])]]).

vuln('ssh-login-root', [ssh, passwords], [root_shell],
        [openssh-[allowrootlogin-(only, ["Yes"])],
        users-[root-(only, [generatePassword])]]).

%vuln('ssh-login(brute-force)', [ssh, user_list], [user_shell],
%        [users-[logins-(exists, [generateUsername]),
%                passwords-(exists, [generatePassword])]]).

vuln('ssh-login-user', [ssh, user_list, passwords], [user_shell],
        [users-[logins-(exists, [generateUsername]),
                passwords-(exists, [generatePassword])]]).

vuln('enumerate-users', [openssh76p1], [user_list], []).

% == Web ==

vuln('scan-http', [], [http],
        [apache-[]]).

vuln('find-login-page', [http], [php_webapp, login_page],
        [apache-[modules-(exists, ["libapache2-mod-php"])],
        php-[deployments-(exists, ["loginpage1"])],
        mysql-[db-(exists, ["logindb1"]),
               root-(only, [generatePassword])]]).

vuln('find-login-page', [http], [php_webapp, login_page, bad_sql],
       [apache-[modules-(exists, ["libapache2-mod-php"])],
        php-[deployments-(exists, ["loginpage1-badsql"])],
        mysql-[db-(exists, ["logindb1"]),
               root-(only, [generatePassword])]]).

%vuln('web-login-admin(brute-force)', [login_page, web_user_list], [web_admin_access, web_passwords], []).

vuln('web-login-admin', [login_page, web_user_list, web_passwords], [web_admin_access], []).

vuln('sql-injection', [login_page, bad_sql], [db_access], []).

vuln('exec-custom-php', [php_webapp, web_admin_access], [user_shell], []).

% == Database ==

vuln('db-query-users', [db_access], [web_user_list, hashed_web_passwords],
        [mysql-[db-(exists, ["logindb1"])]]).

% == Java ==

%vuln('scan-jboss', [http], [jboss],
%        [jboss-[]]).

% CVE-2017-12149
%vuln('deserialization-attack', [jboss], [user_shell],
%        [jboss-[version-(only, ["5.2.2"]),
%                deployments-(exists, ["jbossdemo1.war"])]]).

% == Password cracking ==

vuln('exposed-shadow-file', [user_shell], [hashed_passwords], []).
vuln('exposed-shadow-file', [ftp], [hashed_passwords], []).

vuln('crack-hashes', [hashed_passwords], [passwords], []).
vuln('crack-hashes', [hashed_web_passwords], [web_passwords], []).

% == User shell to root shell (privilege escalation) ==

vuln('scan-for-setuid-binary', [user_shell], [setuid_binary], []).

vuln('examine-setuid-binary', [setuid_binary], [assumed_PATH_var], []).

vuln('custom-PATH-setuid', [user_shell, setuid_binary, assumed_PATH_var], [root_shell], []).

%vuln('scan-for-root-cronjobs', [user_shell], [root_cronjob], []).

%vuln('hijack-root-cronjob', [root_cronjob], [root_shell], []).


