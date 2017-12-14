:- [vulnDatabase]. %import vulnDatabase.pl

allPaths(Goal, InitialState, Result) :-
	setof((Configs, Vulns), achieveGoal(Goal, InitialState, [], [], Configs, Vulns), Result).
allPaths(Goal, InitialState, Attempted, StartingConfigs, Result) :-
	setof((Configs, Vulns), achieveGoal(Goal, InitialState, Attempted, StartingConfigs, Configs, Vulns), Result).

sortByLength(Ordered, (_, Vuln1), (_, Vuln2)) :-
	length(Vuln1, Length1),
	length(Vuln2, Length2),
	compare(Ordered, Length1, Length2).

shortestPath(Goal, InitialState) :-
	allPaths(Goal, InitialState, AllPaths),
	predsort(sortByLength, AllPaths, SortedPaths),
	nth0(0, SortedPaths, Shortest),
	open('plan.txt', write, Stream),
	createPlan(Stream, Shortest), !,
	close(Stream).

longestPath(Goal, InitialState) :-
	allPaths(Goal, InitialState, AllPaths),
	predsort(sortByLength, AllPaths, SortedPaths),
	last(SortedPaths, Longest),
	open('plan.txt', write, Stream),
	createPlan(Stream, Longest), !,
	close(Stream).

createPlan(Stream, (Configs, Vulns)) :-
	writeln(Stream, 'Plan:'),
	writeln(Stream, Vulns),
	writeln(Stream, "\nConfigs:"),
	printConfigs(Stream, Configs).

printConfigs(_, []).
printConfigs(Stream, [Key-Value|Configs]) :-
	write(Stream, Key),
	writeln(Stream, Value),
	printConfigs(Stream, Configs).

runPythonScript() :- shell('python createVars.py').

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
