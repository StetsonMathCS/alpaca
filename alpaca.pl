:- [vulnDatabase]. %import vulnDatabase.pl

allPaths(Goal, InitialState, Result) :-
	setof((Configs, Vulns), (achieveGoal(Goal, InitialState, [], [], Configs, Vulns),
					         checkNoDanglingSteps(Vulns, Vulns, Goal)), Result).
allPaths(Goal, InitialState, Attempted, StartingConfigs, Result) :-
	setof((Configs, Vulns), achieveGoal(Goal, InitialState, Attempted, StartingConfigs, Configs, Vulns), Result).

printPaths([]).
printPaths([Vulns|Rest]) :-
	print(Vulns), nl,
	printPaths(Rest).

allPossiblePaths() :-
	findall((Prereqs, Vuln, Result), vuln(Vuln, Prereqs, Result, _), AllVulns),
	p(AllVulns, Str),
	generateLattice(Str, 'allPossiblePaths-test.gv').

formatGraphviz(_, [], "").
formatGraphviz(VulnID, [(Prereq, Vuln, Result)|Rest], String) :-
	formatGraphviz(VulnID, Rest, String1),
    format(atom(String), "~s\"~a\" [shape=\"none\"];~n\"~a\" [shape=\"none\"];~n\"~s\" [shape=\"box\", label=\"~a\"];~n\"~a\" -> \"~s\";~n\"~s\" -> \"~a\";~n", [String1, Prereq, Result, VulnID, Vuln, Prereq, VulnID, VulnID, Result]).

p([], "").
p([(Prereqs, Vuln, Result)|Rest], Str) :-
	p1(Prereqs, Vuln, Result, [], Out),
    format(atom(VulnID), "~k~a~k", [Prereqs, Vuln, Result]),
	formatGraphviz(VulnID, Out, Str1),
	p(Rest, Str2),
	format(atom(Str), "~s~s", [Str1, Str2]).

p1([], _, Results, Pairs, Out) :- p2(Results, Pairs, Out).
p1([H|T], Vuln, Results, Rest, Out) :-
	p1(T, Vuln, Results, [(H, Vuln)|Rest], Out).

p2([H|T], Pairs, Out) :-
	p3(H, Pairs, Out1),
	p2(T, Pairs, Out2),
	append(Out1, Out2, Out).
p2([], _, []).

p3(Res, [(A,B)|T], [(A, B, Res)|Rest]) :-
	p3(Res, T, Rest).
p3(_, [], []).

generateLattice(String, File) :-
	open(File, write, Stream),
	writeln(Stream, "strict digraph \"Vulnerability Lattice\" {"),
	write(Stream, String),
	write(Stream, "}"),
	close(Stream),
	format(atom(Command), "dot -Tpng ~s > ~s.png", [File, File]),
	shell(Command).

sortByLength(Ordered, (_, Vuln1), (_, Vuln2)) :-
	length(Vuln1, Length1),
	length(Vuln2, Length2),
	compare(Ordered, Length1, Length2).

shortestPath(Goal, InitialState) :-
	allPaths(Goal, InitialState, AllPaths),
	predsort(sortByLength, AllPaths, SortedPaths),
	nth0(0, SortedPaths, (Configs, Vulns)),
	p(Vulns, Str),
	generateLattice(Str, 'shortestPath-test.gv'),
	createYamlFiles(Configs).

longestPath(Goal, InitialState) :-
	allPaths(Goal, InitialState, AllPaths),
	predsort(sortByLength, AllPaths, SortedPaths),
	last(SortedPaths, (Configs, Vulns)),
	p(Vulns, Str),
	generateLattice(Str, 'longestPath-test.gv'),
	createYamlFiles(Configs).

createYamlFiles(Configs) :-
	formatRoles(Configs, Roles),
	createPlaybook(Roles),
	listRoles(Configs, Vars),
	createVars(Vars).

createPlaybook(Roles) :-
	open('playbook.yml', write, Stream),
	format(atom(String), 
		"---~n- hosts: all~n~t~2|become: true~n~t~2|vars_files:~n~t~4|- vars/all.yml~n~t~2|roles:~n~s", [Roles]),
	write(Stream, String),
	close(Stream).

formatRoles([], "").
formatRoles([Role-_|Configs], String) :-
	formatRoles(Configs, String1),
	format(atom(String), "~t~4|- ~s~n~s", [Role, String1]).

createVars(Vars) :- 
	open('all.yml', write, Stream),
	format(atom(String), "---~n~s", [Vars]),
	write(Stream, String),
	close(Stream).

listRoles([], "").
listRoles([Role-Val|Rest], String) :-
	listKeys(Val, String1),
	format(atom(Out), "~s:~n~s", [Role, String1]),
	listRoles(Rest, String2),
	format(atom(String), "~s~s", [Out, String2]).

listKeys([], "").
listKeys([Key-(_, Vals)|Rest], String) :-
	listVals(Vals, String1),
	format(atom(Out), "~t~2|~s:~n~s", [Key, String1]),
	listKeys(Rest, String2),
	format(atom(String), "~s~s", [Out, String2]).

listVals([], "").
listVals([Val|Vals], String) :-
	listVals(Vals, String1),
	format(atom(String), "~t~4|- ~s~n~s", [Val, String1]).

% DON''T CHANGE CODE BELOW HERE. YOU WILL BREAK IT.

%achieveGoal( Goal, InitialState, [Attempted], [Vulns] )
achieveGoal(Goal, InitialState, _, [], [], []) :- member(Goal, InitialState).
achieveGoal(Goal, InitialState, Attempted, StartingConfigs, AcceptedConfigs, [(Input, Description, Output)|Vulns]) :-
		\+member(Goal, InitialState),
		vuln(Description, Input, Output, Configs),
		print(Description), nl,
		\+member((Input, Output, Configs), Attempted),
		intersection(Input, InitialState, Input),
		union(Output, InitialState, NewState),
		print(Description), print('-'), print(InitialState), nl, nl,
		achieveGoal(Goal, NewState, [(Input, Output, Configs)|Attempted], StartingConfigs, NewConfigs, Vulns),
		checkConfigs(NewConfigs, Configs, AcceptedConfigs).

	%how to find all possible paths in prolog graph

ensureVulnLink(Output, _, Goal) :-
	member(Goal, Output).
ensureVulnLink(Output, [(Input, _, _)|_], _) :-
	intersection(Output, Input, [_|_]), !.
ensureVulnLink(Output, [_|Vulns], Goal) :-
	ensureVulnLink(Output, Vulns, Goal).

checkNoDanglingSteps([], _, _).
checkNoDanglingSteps([(_, _, Output)|Vulns], AllVulns, Goal) :-
	ensureVulnLink(Output, AllVulns, Goal),
	checkNoDanglingSteps(Vulns, AllVulns, Goal).


%checkConfigs(AcceptedConfigs, PendingConfigs, NewConfigs)
checkConfigs([], PendingConfigs, PendingConfigs).
checkConfigs([H|T], [], [H|T]).
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
