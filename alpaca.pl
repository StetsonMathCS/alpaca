%vuln( [prereqs], [result], 
%					[Role-[key-(pred,[val]),...,key-(pred,[val])]]

vuln('sql injection', [web_access], [database_queries], 
		 [apache-[version-(only, [2.4])], mysql-[tables-(exists, [users]), version-(only, [5.3])], php-[version-(only, [5.5]), script-(exists, [web])]]).
vuln('db-query-users', [database_queries], [user_list, hashed_passwords], [mysql-[tables-(exists, [users])]]).
vuln('crack passwords', [hashed_passwords], [passwords], []).
vuln('login ssh', [passwords, user_list, ssh_server], [shell_access], []).


%setof((Configs, Vulns), achieve_goal(shell_access, [web_access, ssh_server], [], [], Configs, Vulns), Result), length(Result, L), print(Result).

% DON'T CHANGE CODE BELOW HERE. YOU WILL BREAK IT.

%achieve_goal( Goal, InitialState, [Attempted], [Vulns] )
achieve_goal(Goal, InitialState, _, [], [], []) :- member(Goal, InitialState).
achieve_goal(Goal, InitialState, Attempted, StartingConfigs, AcceptedConfigs, [Description|Vulns]) :-
		vuln(Description, Input, Output, Configs),
		\+member((Input, Output, Configs), Attempted),
		intersection(Input, InitialState, Input),
		union(Output, InitialState, NewState),
		achieve_goal(Goal, NewState, [(Input, Output, Configs)|Attempted], StartingConfigs, NewConfigs, Vulns),
		check_configs(NewConfigs, Configs, AcceptedConfigs).

%check_configs(AcceptedConfigs, PendingConfigs, NewConfigs)
check_configs([], PendingConfigs, PendingConfigs).
check_configs(AcceptedConfigs, [], AcceptedConfigs).
check_configs(AcceptedConfigs, PendingConfigs, SortedConfigs) :-
		select(K-PendingVals, PendingConfigs, RestPendingConfigs),
		\+member(K-_, AcceptedConfigs),
		check_configs(AcceptedConfigs, RestPendingConfigs, TmpConfigs),
		NewConfigs = [K-PendingVals|TmpConfigs],
		sort(NewConfigs, SortedConfigs).
check_configs(AcceptedConfigs, PendingConfigs, SortedConfigs) :-
		select(K-PendingVals, PendingConfigs, RestPendingConfigs),
		select(K-AcceptedVals, AcceptedConfigs, RestAcceptedConfigs),
		check_configs(RestAcceptedConfigs, RestPendingConfigs, TmpConfigs),
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
