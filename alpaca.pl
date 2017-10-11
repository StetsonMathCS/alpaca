% vuln( [prereqs], [result], rolename, tag )
vuln([web_access], [database_queries], sql, initial_web).
vuln([database_queries], [hashed_passwords, user_list], no_effect, sql_query).
vuln([hashed_passwords], [passwords], no_effect, password_cracking).
vuln([passwords, user_list, ssh_server], [shell_access], no_effect, shell_access).

vuln([web_access], [remote_file_inclusion], no_effect, initial_web).
vuln([remote_file_inclusion], [shell_access], no_effect, shell_access).

vuln([login_page], [database_queries], sql, intial_web).

% achieve_goal( Goal, InitialState, [Attempted], [UsedTags], [Vulns] )
% 	vulns gets subtracted from when the recursive case is called, so the base case would be an empty list, and the it would build on its way back up
achieve_goal(Goal, InitialState, _, _, []) :- member(Goal, InitialState).
achieve_goal(Goal, InitialState, Attempted, UsedTags, [(Input, Output, File, Tag)|Vulns]) :-
		vuln(Input, Output, File, Tag),
		\+member((Input, Output, File, Tag), Attempted),
		\+member(Tag, UsedTags),
		intersection(Input, InitialState, Input),
		union(Output, InitialState, NewState),
		achieve_goal(Goal, NewState, [(Input, Output, File, Tag)|Attempted], [Tag|UsedTags], Vulns).

print_rolenames([]).
print_rolenames([(_, _, Role, _)|Vulns]) :-
		nl, print(Role), print_rolenames(Vulns).

write_to_file(Goal, InitialState, Attempted, UsedTags, Vulns) :-
		open('steps.txt', write, Stream),
		forall(achieve_goal(Goal, InitialState, Attempted, UsedTags, Vulns), writeln(Stream, Vulns)),
		close(Stream).


% example: achieve_goal(shell_access, [web_access, ssh_server], [], [], Vulns).
