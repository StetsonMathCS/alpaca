:- [vulnDatabase]. %import vulnDatabase.pl

/*
[([Configs1], [Vulns1]), ([Configs2], [Vulns2]), ..., ([ConfigsN], [VulnsN])]
*/

% find all configs that can merge with the starting config ('Accepted')
filterConfigs(Accepted, [(Config, Vulns)|T], [(FinalConfig, Vulns)|Rest], FinalConfig) :-
    % ensure Accepted doesn't fully contain Config already
    member(K-V, Config),
    \+member(K-V, Accepted),
	checkConfigs(Accepted, Config, Merged),
	filterConfigs(Merged, T, Rest, FinalConfig), !.
filterConfigs(Accepted, [_|T], Rest, FinalConfig) :-
	filterConfigs(Accepted, T, Rest, FinalConfig).
filterConfigs(Config, [], [], Config).

% find subsets of configs that work together (i.e., configs can merge)
configPowerset([], []).
configPowerset([(Config, _)|T], Result) :-
    filterConfigs(Config, T, PathsFiltered, Merged),
	configPowerset(PathsFiltered, P),
    list_to_ord_set([Merged|P], Result).
configPowerset([_|T], P) :-
	configPowerset(T, P).

% NEED TO KEEP ONLY ONE LATTICE (MAXIMAL) PER CONFIG,
% SINCE THAT ONE CONFIG WILL SUPPORT ALL ACTUAL PATHS,
% EVEN IF THE POWERSET DOESN'T INCLUDE ALL PATHS

% e.g., allPaths([server_access_root], [], Lattices)
% paths will be in reverse usually, but that doesn't matter for generating a lattice
allPaths(Goals, InitialState, MergedConfigs) :-
	setof((Configs, Vulns), achieveGoal(Goals, InitialState, [], Configs, Vulns), Paths),
    % find all subsets (powerset) that have compatible configs; don't keep an empty subset
	setof(Configs, configPowerset(Paths, Configs), MergedConfigs).

printLattices([]).
printLattices([First|Rest]) :-
    print('----'), nl,
    printPaths(First), nl, nl,
    printLattices(Rest).

printPaths([]).
printPaths([(Config, Vulns)|Rest]) :-
    print('Config: '), print(Config), nl,
    printVulns(Vulns), nl, nl,
    printPaths(Rest).

printVulns([]).
printVulns([Vulns|Rest]) :-
	print(Vulns), nl,
	printVulns(Rest).

allPossiblePaths :-
	findall((Prereqs, Vuln, Result), vuln(Vuln, Prereqs, Result, _), AllVulns),
	p(AllVulns, Str),
	generateLattice(Str, 'allPossiblePaths-test.gv').

% another way to call formatGraphviz:
% allPaths([server_access_root], [], Result),
%  Result = [(Config, Vulns)|_],  % get first path, just for demostration
%  p(Vulns, Str),
%  generateLattice(Str, 'server_access_root_1.gv').

formatGraphviz(_, [], "").
formatGraphviz(VulnID, [(Prereq, Vuln, Result)|Rest], String) :-
	formatGraphviz(VulnID, Rest, String1),
    ( Prereq = none -> PrereqLabel = '' ; PrereqLabel = Prereq ),
    format(atom(String), "~s\"~a\" [shape=\"none\", label=\"~a\"];~n\"~a\" [shape=\"none\"];~n\"~s\" [shape=\"box\", label=\"~a\"];~n\"~a\" -> \"~s\";~n\"~s\" -> \"~a\";~n", [String1, Prereq, PrereqLabel, Result, VulnID, Vuln, Prereq, VulnID, VulnID, Result]).

p([], "").
p([(Prereqs, Vuln, Result)|Rest], Str) :-
    % if prereqs are empty, put in a dummy [none] so that p1 below doesn't ignore the vuln
    ( Prereqs = [] -> p1([none], Vuln, Result, [], Out) ; p1(Prereqs, Vuln, Result, [], Out) ),
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

% work backwards from goal to initial
%achieveGoal([], _, _, _, []).
achieveGoal([], _, [], [], []).
achieveGoal([Goal|Goals], InitialState, StartingConfigs, AcceptedConfigs, [(Input, Description, Output)|Vulns]) :-
    vuln(Description, Input, Output, Configs),
    member(Goal, Output),
    subtract(Input, InitialState, NewInput),
    union(NewInput, Goals, NewGoals),
    union(InitialState, Output, NewState),
    achieveGoal(NewGoals, NewState, StartingConfigs, NewConfigs, Vulns),
    checkConfigs(NewConfigs, Configs, AcceptedConfigs).

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
