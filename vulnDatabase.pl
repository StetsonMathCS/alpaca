%vuln( Description, [prereqs], [result], [Role-[key-(pred, [val]),...,key-(pred,[val])]] )
%note, result cannot be empty

vuln('host-discovery', [], [discover_host], []).
vuln('port-scanning', [discover_host], [open_ports], []).

% == FTP ==
vuln('open-ftp', [open_ports], [ftp_server], [ftp-[]]).
vuln('directory-traversal', [ftp_server], [user_list], []).
% Password: secret
vuln('login-root(brute-force)', [ftp_server], [server_access_root], 
        [ftp-[root_password-(only, ["'$6$pcWmIiBJypefToKL$OgKL0uQx43wxEf9RkeaOFGkyRJvmJzlj0Farr6to0lLmkMiiG7PPKKvUKky1W0b2LBx93p/EVF2dss79E18J4/'"])]]).

% == SSH ==
vuln('open-ssh', [open_ports], [ssh_server], []).

% Password: qwerty
vuln('login-root(brute-force)', [ssh_server], [server_access_root], 
        [ssh-[root_password-(only, ["'$6$ugnivWA.s5k8bbgi$0zQz2ILZIFmKpCNr5RvVzFn6pYcY7IGZTYr5A3kLRMSrEuOuXrTmw2uhZUU8NAaAxA1Ma9wWHMh2PCT4jjYj20'"])]]).

% Password: qwerty
vuln('login-root(credentials)', [passwords, ssh_server], [server_access_root], 
        [ssh-[root_password-(only, ["'$6$ugnivWA.s5k8bbgi$0zQz2ILZIFmKpCNr5RvVzFn6pYcY7IGZTYr5A3kLRMSrEuOuXrTmw2uhZUU8NAaAxA1Ma9wWHMh2PCT4jjYj20'"])]]).

% Password: password
vuln('login-user(brute-force)', [ssh_server, user_list], [server_access_user], 
        [ssh-[users-(exists, [example]),
              passwords-(exists, ["'$6$zXfK0cjCjTOYGk/t$U1DcfcpU2FQnaT5f/RfRHzRpq.va/d.YuDj7HcNW.B87yhaJgPtuik4CWlWa7drm9oGhZ941OXAtSF7CzXIgP1'"])]]).
vuln('login-user(credentials)', [ssh_server, user_list, passwords], [server_access_user], 
        [ssh-[users-(exists, [example]),
              passwords-(exists, ["'$6$zXfK0cjCjTOYGk/t$U1DcfcpU2FQnaT5f/RfRHzRpq.va/d.YuDj7HcNW.B87yhaJgPtuik4CWlWa7drm9oGhZ941OXAtSF7CzXIgP1'"])]]).

vuln('crack-hashes', [hashed_passwords], [passwords], []).

% Password: 123123
vuln('login-root(credentials)', [passwords, ssh_server], [server_access_root], 
        [ssh-[root_password-(only, ["'$6$XFCpeL8iXtL4MYOx$aDRdDFz941M3tsOHmQWEHicpzotdYQXXY3/eagI5uIRii.moOaKQPv93z6e6uC.9p44PjgVniy9IH1bA.Q0/W.'"])]]).

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
        [ssh-[root_password-(only, ["'$6$Rt0CznBsC/VVsFNT$zIIU0FjskC3QTdWFWro2x1xaQ/BVLJ3d0UKtkw07XPGCQQVVRj5B8kvYe3CSJV.mChqIZ6jKvg.q0CdXuHSzb.'"])]]).

vuln('directory-traversal', [web_access], [bypass_auth], 
        [php-[git_repo-(only, ['alpaca_traversal']),
              repo_folder-(exists, ['alpaca_traversal'])],
        mysql-[db-(exists, ['alpaca_traversal']),
               sql_files-(exists, ['accounts.sql'])]]).
vuln('bypass-authentication(admin)', [bypass_auth], [web_admin_access], []).

%vuln('remote_code_execution', [web_admin_access], [remote_code_execution], []).
%vuln('directory-traversal', [web_server], [remote_code_execution], []).
%vuln('directory-traversal', [web_server], [session_hijack], []).
%vuln('remote_code_execution', [web_admin_access], [remote_code_execution], []).
