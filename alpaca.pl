
vuln([web_access], "SQL Injection", [database_queries]).
vuln([database_queries], "SQL Query", [hashed_passwords, user_list]).
vuln([hashed_passwords], "Password Cracking", [passwords]).
vuln([passwords, user_list, ssh_server], "Shell Access", [shell_access]).

achieve_goal(Goal, InitialState, _, _) :- member(Goal, InitialState).
achieve_goal(Goal, InitialState, Attempted, [(Input, Description, Output)|Vulns]) :-
    vuln(Input, Description, Output),
    \+member((Input, Description, Output), Attempted),
    intersection(Input, InitialState, Input),
    union(Output, InitialState, NewState),
    achieve_goal(Goal, NewState, [(Input, Description, Output)|Attempted], Vulns).

% example: achieve_goal(shell_access, [web_access, ssh_server], [], Vulns).

