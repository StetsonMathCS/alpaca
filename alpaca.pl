:- [vulnDatabase]. %import vulnDatabase.pl
:- [addons].

/*
[([Configs1], [Vulns1]), ([Configs2], [Vulns2]), ..., ([ConfigsN], [VulnsN])]
*/

% successively merge configs (fails if they are not all compatible)
successivelyMergeConfigs(StartingConfig, [(Config, _)|T], FinalMerged) :-
	checkConfigs(StartingConfig, Config, Merged), !,
	successivelyMergeConfigs(Merged, T, FinalMerged).
successivelyMergeConfigs(Config, [], Config).

% assign a new config to each path
updateConfigs(_, [], []).
updateConfigs(Config, [(_, Vulns)|Rest], [(Config, Vulns)|Rest2]) :-
    updateConfigs(Config, Rest, Rest2).

groupPathsByConfigsStep([], []).
groupPathsByConfigsStep([Paths|RestPaths], [UpdatedPaths|RestMerged]) :-
    Paths = [(Config, _)|_],
    select(MatchingPaths, RestPaths, UncheckedPaths),
    append(Paths, MatchingPaths, TestingPaths),
    successivelyMergeConfigs(Config, TestingPaths, Merged),
    updateConfigs(Merged, TestingPaths, UpdatedPaths),
    groupPathsByConfigsStep(UncheckedPaths, RestMerged).
groupPathsByConfigsStep([Paths|RestPaths], [Paths|RestMerged]) :-
    groupPathsByConfigsStep(RestPaths, RestMerged).

groupPathsByConfigs(Paths, Result) :-
    groupPathsByConfigsStep(Paths, NewPaths),
    dif(Paths, NewPaths), !,
    groupPathsByConfigs(NewPaths, Result).
groupPathsByConfigs(Paths, Paths).

% e.g., allPaths([server_access_root], [], Lattices)
% paths will be in reverse usually, but that doesn't matter for generating a lattice
% result (Lattices) will have structure: [Lattice|...],
% where each Lattice has the structure: [(Config, Vulns)|...],
% where Config is a maximally merged config for the lattice (all paths in the
% lattice will have this same maximal config)
allPaths(Goals, InitialState, Lattices) :-
	setof([(Config, Vulns)], achieveGoal(Goals, InitialState, [], Config, Vulns), Paths),
    % repeatedly merge these configs until no more merging is possible
    groupPathsByConfigs(Paths, Lattices).

printLattices([]).
printLattices([Lattice|Rest]) :-
    print('----'), nl,
    printLattice(Lattice), nl, nl,
    printLattices(Rest).

printLattice([]).
printLattice([(Config, Vulns)|Rest]) :-
    print('Config: '), print(Config), nl,
    printVulns(Vulns), nl, nl,
    printLattice(Rest).

printVulns([]).
printVulns([Vulns|Rest]) :-
	print(Vulns), nl,
	printVulns(Rest).

allPossiblePaths :-
	findall((Prereqs, Vuln, Result), vuln(Vuln, Prereqs, Result, _), AllVulns),
	p(AllVulns, Str),
	generateLattice(Str, 'allPossiblePaths.gv').

% another way to call formatGraphviz:
% allPaths([server_access_root], [], Result),
%  Result = [(Config, Vulns)|_],  % get first path, just for demonstration
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

% Finds all lattices, create directories, generate lattices in directory, create ansible playbooks
% Example: createAllPaths(['server_access_root'], [], 'server_access_root')
createAllPaths(Goal, InitialState, Name) :-
	allPaths(Goal, InitialState, Lattices),
	length(Lattices, Length),
	createLatticeDirectories(Name, 1, Length),
	generateLatticeInDirectory(Lattices, Name),
	getConfigs(Lattices, Name, 1).

% creates new directory for each lattice
createLatticeDirectories(_, Num, Length) :- Num > Length, !.
createLatticeDirectories(Name, Num, Length) :-
	number_string(Num, NumString),
	format(atom(DirectoryName), "~s~s", [Name, NumString]),
	exists_directory(DirectoryName), !,
	NewNum is Num+1,
	createLatticeDirectories(Name, NewNum, Length).
createLatticeDirectories(Name, Num, Length) :-
	Num =< Length,
	number_string(Num, NumString),
	format(atom(DirectoryName), "~s~s", [Name, NumString]),
	make_directory(DirectoryName),
	NewNum is Num+1,
	createLatticeDirectories(Name, NewNum, Length).

% Generates all graphs from list of lattices with FileName
% This predicate
% Example: generateAllGraphs([server_access_root], [], 'server_access_root')
generateAllGraphs(Goal, InitialState, FileName) :-
	allPaths(Goal, InitialState, Lattices),
	generateAllLattices(Lattices, FileName).

generateAllLattices([], _).
generateAllLattices(Lattices, FileName) :-
	generateAllLattices(Lattices, FileName, 1).

generateAllLattices([], _, _).
generateAllLattices([Lattice|Lattices], FileName, Num) :-
	appendVulns(Lattice, ListOfVulns),
	append(ListOfVulns, Result),
	number_string(Num, NumString),
	p(Result, Str),
	format(atom(NewFileName), "~s~s.gv", [FileName, NumString]),
	generateLattice(Str, NewFileName), !,
	NewNum is Num+1,
	generateAllLattices(Lattices, FileName, NewNum).

% Generates lattices in the directory where it belongs
generateLatticeInDirectory([], _).
generateLatticeInDirectory(Lattices, DirectoryName) :-
	generateLatticeInDirectory(Lattices, DirectoryName, 1).

generateLatticeInDirectory([], _, _).
generateLatticeInDirectory([Lattice|Lattices], DirectoryName, Num) :-
	appendVulns(Lattice, ListOfVulns),
	append(ListOfVulns, Result),
	number_string(Num, NumString),
	p(Result, Str),
	format(atom(NewDirectoryName), "~s~s/lattice.gv", [DirectoryName, NumString]),
	generateLattice(Str, NewDirectoryName), !,
	NewNum is Num+1,
	generateLatticeInDirectory(Lattices, DirectoryName, NewNum).

appendVulns([], []).
appendVulns([(_, Vulns)|RestPaths], [Vulns|Result]) :-
	appendVulns(RestPaths, Result).

% HEADER [shape="none" label="This is the header"];
% Calculates Complexities of all lattices in list of lattices and gives back list of complexities
% Index of complexity corresponds to index of lattice
calculateLatticeComplexity([], []).
calculateLatticeComplexity([Lattice|Lattices], [LatticeComplexity|Rest]) :-
	calculatePathComplexity(Lattice, Complexity),
	sum_list(Complexity, Sum),
	LatticeComplexity is 1/Sum,
	calculateLatticeComplexity(Lattices, Rest).

calculatePathComplexity([], []).
calculatePathComplexity([(_, Vulns)|RestPaths], [Complexity|Rest]) :-
	length(Vulns, Length),
	Complexity is 1/Length,
	calculatePathComplexity(RestPaths, Rest).

sortByLength(Ordered, (_, Vulns1), (_, Vulns2)) :-
	length(Vulns1, Length1),
	length(Vulns2, Length2),
	compare(Ordered, Length1, Length2).

% gives back shortest path in each lattice
% Example: allPaths([server_access_root], [], Lattices), shortestPathInLattices(Lattices, Shortest)
shortestPathInLattices([], []).
shortestPathInLattices([Lattice|Lattices], [[Shortest]|Rest]) :-
	predsort(sortByLength, Lattice, SortedPaths),
	nth0(0, SortedPaths, Shortest),
    shortestPathInLattices(Lattices, Rest).

% Constrains results to a minimum length
% Example: allPaths([server_access_root], [], Lattices), filterLatticesByShortest(10, Lattices, Result)
latticeShortestPath(_, []).
latticeShortestPath(MinLength, [(_,Path)|Paths]) :-
    length(Path, L),
    L >= MinLength,
    latticeShortestPath(MinLength, Paths).

filterLatticesByShortest(_, [], []).
filterLatticesByShortest(MinLength, [Lattice|Lattices], [Lattice|Result]) :-
    latticeShortestPath(MinLength, Lattice), !,
    filterLatticesByShortest(MinLength, Lattices, Result), !.
filterLatticesByShortest(MinLength, [_|Lattices], Result) :-
    filterLatticesByShortest(MinLength, Lattices, Result).


% BROKEN: allPaths returns a list of paths, not just paths,
% since we are now grouping paths by their configs (i.e., making distinct lattices)
shortestPath(Goal, InitialState) :-
	allPaths(Goal, InitialState, AllPaths),
	predsort(sortByLength, AllPaths, SortedPaths),
	nth0(0, SortedPaths, (Configs, Vulns)),
	p(Vulns, Str),
	generateLattice(Str, 'shortestPath-test.gv'),
	createYamlFiles(Configs).

% BROKEN: allPaths returns a list of paths, not just paths,
% since we are now grouping paths by their configs (i.e., making distinct lattices)
longestPath(Goal, InitialState) :-
	allPaths(Goal, InitialState, AllPaths),
	predsort(sortByLength, AllPaths, SortedPaths),
	last(SortedPaths, (Configs, Vulns)),
	p(Vulns, Str),
	generateLattice(Str, 'longestPath-test.gv'),
	createYamlFiles(Configs).

getConfigs([], _, _).
getConfigs([Lattice|Lattices], Name, Num) :-
	nth0(0, Lattice, First),
	getConfig(First, Name, Num),
	NewNum is Num+1,
	getConfigs(Lattices, Name, NewNum).

getConfig([], _, _).
getConfig((Configs, _), Name, Num) :-
	createYamlFiles(Configs, Name, Num).

createYamlFiles(Configs, Name, Num) :-
	formatRoles(Configs, Roles),
	createPlaybook(Roles, Name, Num),
	number_string(Num, NumString),
	format(atom(DirectoryName), "~s~s/vars", [Name, NumString]),
	make_directory(DirectoryName),
	listRoles(Configs, Vars),
	createVars(Vars, Name, Num).

createPlaybook(Roles, Name, Num) :-
	number_string(Num, NumString),
	format(atom(DirectoryName), "~s~s/playbook.yml", [Name, NumString]),
	open(DirectoryName, write, Stream),
	%open('playbook.yml', write, Stream),
	format(atom(String),
		"---~n- hosts: all~n~t~2|become: true~n~t~2|vars_files:~n~t~4|- vars/all.yml~n~t~2|roles:~n~s", [Roles]),
	write(Stream, String),
	close(Stream).

formatRoles([], "").
formatRoles([Role-_|Configs], String) :-
	formatRoles(Configs, String1),
	format(atom(String), "~t~4|- ~s~n~s", [Role, String1]).

createVars(Vars, Name, Num) :-
	number_string(Num, NumString),
	format(atom(DirectoryName), "~s~s/vars/all.yml", [Name, NumString]),
	open(DirectoryName, write, Stream),
	%open('all.yml', write, Stream),
	format(atom(String), "---~n~s", [Vars]),
	write(Stream, String),
	close(Stream).

listRoles([], "").
listRoles([Role-Val|Rest], String) :-
	listKeys(Val, String1),
	format(atom(Out), "~s:~n~s~n", [Role, String1]),
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
listVals(Predicate, String) :-
	call(Predicate, Output),
	format(atom(String), "~t~4|- ~s~n", [Output]).

% Creates ansible/playbook.yml file
% Starts vagrant to generate range
createRange(DirectoryName) :-
	open('ansible/playbook.yml', write, Stream),
	format(atom(String), "---~n- import_playbook: ../~s/playbook.yml", [DirectoryName]),
	write(Stream, String),
	close(Stream),
	shell('vagrant up').

% work backwards from goal to initial
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
    sort(AllVals, SortedVals),
    Result = (NewPred, SortedVals).

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

% MySQL Commands
query(USER, PWD, DB, QUERY, Columns, Rows) :-
	atom_concat('-p', PWD, PPWD),
	process_create(path(mysql), ['-u', USER, PPWD, '-D', DB, '-e', QUERY], [stdout(pipe(Out)),stderr(std)]),
	read_record(Out, Columns),
	read_records(Out, Rows).

read_record(Out, Fields) :-
	read_line_to_codes(Out, Codes),
	Codes \= end_of_file,
	atom_codes(Line, Codes),
	atomic_list_concat(Fields, '\t', Line).

read_records(Out, [Record|Rs]) :-
	read_record(Out, Record),
	!, read_records(Out, Rs).
read_records(Out, []) :-
	close(Out).

% convert MySQL table into ProLog knowledge base
capture_table(USER, PWD, DB, QUERY, Functor) :-
	query(USER, PWD, DB, QUERY, _Columns, Rows),
	maplist(capture_table(Functor), Rows).

capture_table(Functor, Row) :-
	Clause =.. [Functor|Row],
	assertz(Clause).

% creates a lattice w/ complexity specified in the specific bounds.
createLatticeWithComplexityIGS(Goal, InitialState, Lower, Upper, Name) :-
	findLatticeWithComplexityIGS(Goal, InitialState, Lower, Upper, Lattice),
	createLatticeDirectories(Name, 1, 1),
	generateLatticeInDirectory([Lattice], Name),
	getConfigs([Lattice], Name, 1).

findLatticeWithComplexityIGS(Goal, InitialState, Lower, Upper, Lattice) :-
	createAllLatticesFromIGS(Goal, InitialState, Lattices),
	calculateLatticeComplexity(Lattices, Sums),
	matchBoundedConstraint(Sums, Lower, Upper, Elem),
	indexOf(Sums, Elem, Index),
	nth0(Index, Lattices, Lattice).

matchBoundedConstraint([L|_], Lower, Upper, Index) :-
	L =< Upper,
	L >= Lower,
	Index = L,
	!.

matchBoundedConstraint([_|O], Lower, Upper, Index) :-
	matchBoundedConstraint(O, Lower, Upper, Index).

indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
	indexOf(Tail, Element, Index1),
	!,
	Index is Index1+1.

% create a vulnerability lattice, constraining for
% a specific Vulnerability
createLatticeWithVulnIGS(Goal, InitialState, Name, Vuln) :-
	createAllLatticesFromIGS(Goal, InitialState, Lattices),
	checkVulnLatticesForVuln(Lattices, Vuln, Lattice),
	createLatticeDirectories(Name, 1, 1),
	generatePNGFromDotInDirectory([Lattice], Name),
	getConfigs([Lattice], Name, 1).

checkVulnLatticesForVuln([Lattice|_], Vuln, LatticeR) :-
	checkVulnAndConfig(Lattice, Vuln),
	LatticeR = Lattice.

checkVulnLatticesForVuln([_|Rest], Vuln, LatticeR) :-
	checkVulnLatticesForVuln(Rest, Vuln, LatticeR).

checkVulnAndConfig([(Config, Vulns)|_], Vuln) :-
	checkVulns(Vulns, Vuln).

checkVulnAndConfig([_|Rest], Vuln) :-
	checkVulnAndConfig(Rest, Vuln).

checkVulns([Vulns|_], Vuln) :-
	with_output_to(atom(Ato), write(Vulns)),
	sub_atom(Ato, B, L, A, Vuln).

checkVulns([_|Rest], Vuln) :-
	checkVulns(Rest, Vuln).

% generates an atom from a list of atoms, at random
generateFromList(List, Length, Output) :-
	length(Output, Length),
	length(List, N1),
	maplist(random_char_generate(List, N1), Output).

random_char_generate(List, N, Char):-  random(0, N, X), nth0(X, List, Char).

% generates a username from username list, defined in vulnDatabase.pl
generateUsername(Username) :-
	usernames(Usernames),
	generateFromList(Usernames, 1, Output),
	nth0(0, Output, Elem),
	Username = Elem.

% generates a password, pulling letters from a dictionary
generatePasswordOfLength(Length, Password) :-
  passwords(Passwords),
	generateFromList(Passwords, Length, Output),
	atom_chars(GenPwd, Output),
	Password = GenPwd.
