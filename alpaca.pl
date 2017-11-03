%vuln( [prereqs], [result], 
%					[Role-[key-(pred,[val]),...,key-(pred,[val])]]

vuln('sql-injection', [web_access], [database_queries], 
			[apache-[version-(only, [2.4])], mysql-[tables-(exists, [users]), version-(only, [5.3])], php-[version-(only, [5.5]), script-(exists, [web])]]).
vuln('db-query-users', [database_queries], [user_list, hashed_passwords], [mysql-[tables-(exists, [users])]]).
vuln('crack-passwords', [hashed_passwords], [passwords], []).
vuln('login-ssh', [passwords, user_list, ssh_server], [shell_access], []).

vuln('login-verbose', [web_access, login_page_verbose], [unauthorized_access], 
			[apache-[version-(only, [2.4])], mysql-[tables-(exists, [users])], php-[version-(only, [5.5]), script-(exists, [web_login_verbose, web])]]).

vuln('login-cracked-passwords', [web_access, login_page, passwords, user_list], [unauthorized_access], 
			[apache-[version-(only, [2.4])], mysql-[tables-(exists, [users])], php-[version-(only, [5.5]), script-(exists, [web_login_cracked_passwords])]]).

%setof((Configs, Vulns), achieveGoal(shell_access, [web_access, ssh_server], [], [], Configs, Vulns), Result), length(Result, L), print(Result).


getPlan(Goal, InitialState, Result) :-
		setof((Configs, Vulns), achieveGoal(Goal, InitialState, [], [], Configs, Vulns), Result).
getPlan(Goal, InitialState, Attempted, StartingConfigs, Result) :-
		setof((Configs, Vulns), achieveGoal(Goal, InitialState, Attempted, StartingConfigs, Configs, Vulns), Result).

savePlan(Goal, InitialState) :-
		open('plan.txt', write, Stream),
		getPlan(Goal, InitialState, Result),
		formatConfigs(Stream, Result),
		close(Stream).
savePlan(Goal, InitialState, Attempted, StartingConfigs) :-
		open('plan.txt', write, Stream),
		getPlan(Goal, InitialState, Attempted, StartingConfigs, Result),
		formatConfigs(Stream Result),
		close(Stream).

% "Return" only Configurations of a plan
getConfigs([(Configs, _)], Configs).

% "Return only Vulnerabilities of a plan
getVulns([(_, Vulns)], Vulns).

formatConfigs(Stream, Results) :-
		getConfigs(Results, Configs),
		printConfigs(Stream, Configs).

printConfigs(_, []).
printConfigs(Stream, [Config|Configs]) :-
		writeln(Stream, Config),
		printConfigs(Stream, Configs).

% DON'T CHANGE CODE BELOW HERE. YOU WILL BREAK IT.

%achieveGoal( Goal, InitialState, [Attempted], [Vulns] )
achieveGoal(Goal, InitialState, _, [], [], []) :- member(Goal, InitialState).
achieveGoal(Goal, InitialState, Attempted, StartingConfigs, AcceptedConfigs, [Description|Vulns]) :-
		vuln(Description, Input, Output, Configs),
		\+member((Input, Output, Configs), Attempted),
		intersection(Input, InitialState, Input),
		union(Output, InitialState, NewState),
		achieveGoal(Goal, NewState, [(Input, Output, Configs)|Attempted], StartingConfigs, NewConfigs, Vulns),
		checkConfigs(NewConfigs, Configs, AcceptedConfigs).

%checkConfigs(AcceptedConfigs, PendingConfigs, NewConfigs)
checkConfigs([], PendingConfigs, PendingConfigs).
checkConfigs(AcceptedConfigs, [], AcceptedConfigs).
checkConfigs(AcceptedConfigs, PendingConfigs, SortedConfigs) :-
		select(K-PendingVals, PendingConfigs, RestPendingConfigs),
		\+member(K-_, AcceptedConfigs),
		checkConfigs(AcceptedConfigs, RestPendingConfigs, TmpConfigs),
		NewConfigs = [K-PendingVals|TmpConfigs],
		sort(NewConfigs, SortedConfigs).
checkConfigs(AcceptedConfigs, PendingConfigs, SortedConfigs) :-
		select(K-PendingVals, PendingConfigs, RestPendingConfigs),
		select(K-AcceptedVals, AcceptedConfigs, RestAcceptedConfigs),
		checkConfigs(RestAcceptedConfigs, RestPendingConfigs, TmpConfigs),
		mergeConfigs(AcceptedVals, PendingVals, MergedConfigs),
		NewConfigs = [K-MergedConfigs|TmpConfigs],
		sort(NewConfigs, SortedConfigs).

only(PriorVals, _, ThisVals, _, _, only) :-
    union(PriorVals, ThisVals, AllVals),
    length(AllVals, L),
    L =< 1.

exists(_, exists, _, _, _, exists).
exists(PriorVals, only, ThisVals, _, _, only) :-
    union(PriorVals, ThisVals, PriorVals).

mergeConfig(Key, (Pred, PriorVals), (ThisPred, ThisVals), Config, Result) :-
    Check =.. [Pred, PriorVals, ThisPred, ThisVals, Key, Config, NewPred],
    call(Check),
    union(PriorVals, ThisVals, AllVals),
    Result = (NewPred, AllVals).

mergeConfigs([], ThisConfig, ThisConfig).
mergeConfigs(PriorConfig, [], PriorConfig).
mergeConfigs(PriorConfig, ThisConfig, SortedConfig) :-
    select(K-ThisVals, ThisConfig, RestThisConfig),
    \+member(K-_, PriorConfig),
    mergeConfigs(PriorConfig, RestThisConfig, TmpConfig),
    NewConfig = [K-ThisVals|TmpConfig],
		sort(NewConfig, SortedConfig).
mergeConfigs(PriorConfig, ThisConfig, SortedConfig) :-
    select(K-ThisVals, ThisConfig, RestThisConfig),
    select(K-PriorVals, PriorConfig, RestPriorConfig),
    mergeConfigs(RestPriorConfig, RestThisConfig, TmpConfig),
    mergeConfig(K, PriorVals, ThisVals, PriorConfig, NewVals),
    NewConfig = [K-NewVals|TmpConfig],
		sort(NewConfig, SortedConfig).

