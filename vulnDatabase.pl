%vuln( Description, [prereqs], [result], [Role-[key-(pred, [val]),...,key-(pred,[val])]] )
%note, result cannot be empty

vuln('host-discovery', [], [discover_host], []).
vuln('port-scanning', [discover_host], [open_ports], []).

% == FTP ==
vuln('open-ftp', [open_ports], [ftp_server], [ftp-[]]).
vuln('directory-traversal', [ftp_server], [user_list], []).
% Password: secret
vuln('login-root(brute-force)', [ftp_server], [server_access_root],
        [ftp-[root_password-(only, generatePasswordOfLength(15))]]).

% == SSH ==
vuln('open-ssh', [open_ports], [ssh_server], []).

% Password: qwerty
vuln('login-root(brute-force)', [ssh_server], [server_access_root],
        [ssh-[root_password-(only, generatePasswordOfLength(15))]]).

% Password: qwerty
vuln('login-root(credentials)', [passwords, ssh_server], [server_access_root],
        [ssh-[root_password-(only, generatePasswordOfLength(15))]]).

vuln('crack-hashes', [hashed_passwords], [passwords], []).

% Password: 123123
vuln('login-root(credentials)', [passwords, ssh_server], [server_access_root],
        [ssh-[root_password-(only, generatePasswordOfLength(15))]]).

% == Web ==
vuln('open-web', [open_ports], [port_80], []).
vuln('web-access', [port_80], [web_access],
        [apache-[]]).

vuln('login-admin(brute-force)', [web_access, login_page], [web_admin_access],
        [php-[git_repo-(exists, ['brute-force']),
              repo_folder-(exists, ['alpaca_bruteforce'])],
        mysql-[db-(exists, ['alpaca_bruteforce']),
               sql_files-(exists, ['accounts.sql'])]]).

vuln('login-admin(credentials)', [web_access, login_page], [web_admin_access],
        [php-[git_repo-(exists, ['alpaca_bruteforce']),
              repo_folder-(exists, ['alpaca_bruteforce'])],
        mysql-[db-(exists, ['alpaca_bruteforce']),
               sql_files-(exists, ['accounts.sql'])]]).

vuln('sql-injection', [login_page], [database_queries],
        [php-[git_repo-(exists, ['https://KimAChen@bitbucket.org/KimAChen/alpaca_sqli.git']),
              repo_folder-(exists, ['alpaca_sqli'])],
        mysql-[db-(exists, ['alpaca_sqli']),
               sql_files-(exists, ['users.sql'])]]).
vuln('db-query-users', [database_queries], [user_list, hashed_passwords],
        [mysql-[users-(exists, ['alpaca_sqli']),
                passwords-(exists, ['password'])]]).
vuln('crack-hashes', [hashed_passwords], [passwords], []).

% Password: 123456789
vuln('login-root(credentials)', [passwords, ssh_server], [server_access_root],
        [ssh-[root_password-(only, generatePasswordOfLength(15))]]).

vuln('directory-traversal', [web_access], [bypass_auth],
        [php-[git_repo-(only, ['alpaca_traversal']),
              repo_folder-(exists, ['alpaca_traversal'])],
        mysql-[db-(exists, ['alpaca_traversal']),
               sql_files-(exists, ['accounts.sql'])]]).
vuln('bypass-authentication(admin)', [bypass_auth], [web_admin_access], []).

passwords(['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
          'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
          'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '1', '2', '3', '4',
          '5', '6', '7', '8', '9', '0', '!', '@', '#', '$', '%' ,'^', '&', '*', '(', ')']).

%vuln('remote_code_execution', [web_admin_access], [remote_code_execution], []).
%vuln('directory-traversal', [web_server], [remote_code_execution], []).
%vuln('directory-traversal', [web_server], [session_hijack], []).
%vuln('remote_code_execution', [web_admin_access], [remote_code_execution], []).
