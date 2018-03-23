%vuln( Description, [prereqs], [result], [Role-[key-(pred, [val]),...,key-(pred,[val])]] )
%note, result cannot be empty

vuln('host-discovery', [], [discover_host], []).
vuln('port-scanning', [discover_host], [open_ports], []).

vuln('open-ftp', [open_ports], [ftp_server], [ftp-[]]).
vuln('login-root(brute-force)', [ftp_server], [server_access_root], []).

vuln('open-ssh', [open_ports], [ssh_server], []).
vuln('login-root(brute-force)', [ssh_server], [server_access_root], 
        [ssh-[users-(exists, root)]]).
vuln('login-user(brute-force)', [ssh_server], [server_access_user], 
        [ssh-[users-(exists, user)]]).

vuln('database_queries', [server_access_user], [user_list, hashed_passwords], []).
vuln('crack-hashes', [hashed_passwords], [passwords], []).
vuln('login-root(credentials)', [passwords, ssh_server], [server_access_root], []).

vuln('open-web', [open_ports], [port_80], []).

vuln('web-access', [port_80], [web_access], []).

vuln('remote_code_execution', [web_admin_access], [remote_code_execution], []).
vuln('directory-traversal', [web_access], [remote_code_execution], []).

vuln('directory-traversal', [web_access], [bypass_auth], []).
vuln('bypass-authentication(admin)', [bypass_auth], [web_admin_access], []).
vuln('login-admin(brute-force)', [web_access, login_page], [web_admin_access], []).

vuln('sql-injection', [web_access], [database_queries], []).
vuln('db-query-users', [database_queries], [user_list, hashed_passwords], []).
vuln('crack-hashes', [hashed_passwords], [passwords], []).
vuln('login-admin(credentials)', [web_access, login_page], [web_admin_access], []).

vuln('directory-traversal', [web_access], [remote_code_execution], []).
vuln('directory-traversal', [web_access], [bypass_auth], []).
vuln('directory-traversal', [web_access], [session_hijack], []).

vuln('bypass-authentication(admin)', [bypass_auth], [web_admin_access], []).
vuln('remote_code_execution', [web_admin_access], [remote_code_execution], []).
