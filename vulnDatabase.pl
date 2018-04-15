%vuln( Description, [prereqs], [result], [Role-[key-(pred, [val]),...,key-(pred,[val])]] )
%note, result cannot be empty

vuln('host-discovery', [], [discover_host], []).
vuln('port-scanning', [discover_host], [open_ports], []).

% FTP
vuln('open-ftp', [open_ports], [ftp_server], [ftp-[]]).
vuln('login-root(brute-force)', [ftp_server], [server_access_root], 
        [ftp-[root_password-(only, ['password'])]]).

% SSH
vuln('open-ssh', [open_ports], [ssh_server], 
        [role-[value-(only, [ssh])]]).

vuln('login-root(brute-force)', [ssh_server], [server_access_root], 
        [ssh-[root_password-(only, ['secret'])]]).

vuln('login-root(credentials)', [passwords, ssh_server], [server_access_root], 
        [ssh-[root_password-(only, ['secret'])]]).

vuln('login-user(brute-force)', [ssh_server], [server_access_user], 
        [ssh-[users-(exists, [example]),
              passwords-(exists, ['password'])]]).

vuln('database_queries', [server_access_user], [user_list, hashed_passwords], 
        [mysql-[users-(exists, [example]),
                passwords-(exists, ['password'])]]).
vuln('crack-hashes', [hashed_passwords], [passwords], []).
vuln('login-root(credentials)', [passwords, ssh_server], [server_access_root], 
        [ssh-[root_password-(only, ['secret'])]]).

% Web
vuln('open-web', [open_ports], [port_80], 
        [role-[value-(only, [web])]]).
vuln('web-access', [port_80], [web_access], 
        [apache-[]]).

vuln('login-admin(brute-force)', [web_access, login_page], [web_admin_access], []).

vuln('login-admin(credentials)', [web_access, login_page], [web_admin_access], []).

vuln('sql-injection', [web_access], [database_queries], 
        [php-[git_repo-(exists, ['https://KimAChen@bitbucket.org/KimAChen/alpaca_sqli.git']),
              repo_folder-(exists, ['alpaca_sqli']),
              mysql_db-(exists, ['alpaca_sqli']),
              mysql_file-(exists, ['users.sql'])]]).
vuln('db-query-users', [database_queries], [user_list, hashed_passwords], 
        [mysql-[user-(exists, ['alpaca_sqli']), 
                password-(exists, ['password'])]]).
vuln('crack-hashes', [hashed_passwords], [passwords], []).
vuln('login-root(credentials)', [passwords, ssh_server], [server_access_root], 
        [ssh-[root_password-(only, ['secret'])]]).

vuln('directory-traversal', [web_access], [bypass_auth], []).
vuln('bypass-authentication(admin)', [bypass_auth], [web_admin_access], []).

%vuln('remote_code_execution', [web_admin_access], [remote_code_execution], []).
%vuln('directory-traversal', [web_access], [remote_code_execution], []).
%vuln('directory-traversal', [web_access], [session_hijack], []).
%vuln('remote_code_execution', [web_admin_access], [remote_code_execution], []).