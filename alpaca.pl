% :- [vulnDatabase].
%:- use_module(library(archive)).

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

% e.g., createAllLatticesFromIGS([server_access_root], [], Lattices)
% paths will be in reverse usually, but that doesnt matter for generating a lattice
% result (Lattices) will have structure: [Lattice|...],
% where each Lattice has the structure: [(Config, Vulns)|...],
% where Config is a maximally merged config for the lattice (all paths in the
% lattice will have this same maximal config)
% renamed from 'allPaths'
createAllLatticesFromIGS(Goals, InitialState, Lattices) :-
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

% renamed from 'allPossiblePaths'
graphAllVulns(FileName) :-
	findall((Prereqs, Vuln, Result), vuln(Vuln, Prereqs, Result, _), AllVulns),
	formatDotVulns(AllVulns, Str),
	generatePNGFromDot(Str, FileName), !.

% another way to call formatDotSingleVuln:
% createAllLatticesFromIGS([server_access_root], [], Result),
%  Result = [(Config, Vulns)|_],  % get first path, just for demonstration
%  formatDotVulns(Vulns, Str),
%  generatePNGFromDot(Str, 'allLattices.gv').

formatDotSingleVuln(_, [], "").
formatDotSingleVuln(VulnID, [(Prereq, Vuln, Result)|Rest], String) :-
	formatDotSingleVuln(VulnID, Rest, String1),
    ( Prereq = none -> PrereqLabel = '' ; PrereqLabel = Prereq ),
    format(atom(String), "~s\"~a\" [shape=\"none\", label=\"~a\"];~n\"~a\" [shape=\"none\"];~n\"~s\" [shape=\"box\", label=\"~a\"];~n\"~a\" -> \"~s\";~n\"~s\" -> \"~a\";~n", [String1, Prereq, PrereqLabel, Result, VulnID, Vuln, Prereq, VulnID, VulnID, Result]).

% renamed from 'p'
formatDotVulns([], "").
formatDotVulns([(Prereqs, Vuln, Result)|Rest], Str) :-
    % if prereqs are empty, put in a dummy [none] so that makeTripletsFromListAtomList below doesnt ignore the vuln
    ( Prereqs = [] -> makeTripletsFromListAtomList([none], Vuln, Result, [], Out) ; makeTripletsFromListAtomList(Prereqs, Vuln, Result, [], Out) ),
    format(atom(VulnID), "~k~a~k", [Prereqs, Vuln, Result]),
	formatDotSingleVuln(VulnID, Out, Str1),
	formatDotVulns(Rest, Str2),
	format(atom(Str), "~s~s", [Str1, Str2]).

% renamed from 'p1'
makeTripletsFromListAtomList([], _, Results, Pairs, Out) :- addAtomIdToEndOfEachPair(Results, Pairs, Out).
makeTripletsFromListAtomList([H|T], Vuln, Results, Rest, Out) :-
	makeTripletsFromListAtomList(T, Vuln, Results, [(H, Vuln)|Rest], Out).

% renamed from 'p2'
addAtomIdToEndOfEachPair([H|T], Pairs, Out) :-
	addAtomToEndOfEachPair(H, Pairs, Out1),
	addAtomIdToEndOfEachPair(T, Pairs, Out2),
	append(Out1, Out2, Out).
addAtomIdToEndOfEachPair([], _, []).

% renamed from 'p3'
addAtomToEndOfEachPair(Res, [(A,B)|T], [(A, B, Res)|Rest]) :-
	addAtomToEndOfEachPair(Res, T, Rest).
addAtomToEndOfEachPair(_, [], []).

% renamed from 'generateLattice'
generatePNGFromDot(String, File) :-
	open(File, write, Stream),
	writeln(Stream, "strict digraph \"Vulnerability Lattice\" {"),
	write(Stream, String),
	write(Stream, "}"),
	close(Stream),
	format(atom(Command), "dot -Tpng ~s > ~s.png", [File, File]),
	shell(Command).


% Example: createRangeFromIGS(['server_access_root'], [], 'server_access_root')
% Finds all lattices, create directories, generate lattices in directory, create ansible playbooks
% renamed from 'createRange'
createRangeFromIGS(Goal, InitialState, DirectoryName, Params) :-
    createAllLatticesFromIGS(Goal, InitialState, Lattices),
    length(Lattices, Length),
    createLatticeDirectories(DirectoryName, 1, Length),
    generatePNGFromDotInDirectory(Lattices, DirectoryName),
    getConfigs(Lattices, DirectoryName, 1, Params),
    format(atom(NewDirectoryName), "~s~s", [DirectoryName, "1"]),
    open('ansible/playbook.yml', write, Stream),
    format(atom(String), "---~n- import_playbook: ../~s/playbook.yml", [NewDirectoryName]),
    write(Stream, String),
    close(Stream).

% Initializes the range as a virtual machine

% renamed from 'createAllPaths'
createStartRangeFromIGS(VMname) :-
    format(atom(Command1), "vagrant up ~s", [VMname]),
    format(atom(Command2), "vagrant halt ~s", [VMname]),
    shell(Command1),
    shell(Command2).
	%shell('vagrant up sr_range_1'),
    %shell('vagrant halt sr_range_1').


createStartRangeMultiple(List):-
    length(List, N),
    foreach(between(1,N,X), startStopRangeMultiple(List, X)).

startStopRangeMultiple(List, X):-
    indexOf(List, VMname, X),
    createStartRangeFromIGS(VMname).


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


generatePNGFromLattices([], _).
generatePNGFromLattices(Lattices, FileName) :-
	generatePNGFromLattices(Lattices, FileName, 1).

% Generates all graphs from list of lattices with FileName
% This predicate
% Example: generateAllGraphs([server_access_root], [], 'server_access_root')
generateAllGraphs(Goal, InitialState, FileName) :-
	allPaths(Goal, InitialState, Lattices),
	generateAllLattices(Lattices, FileName).

generateAllLattices([], _).
generateAllLattices(Lattices, FileName) :-
	generateAllLattices(Lattices, FileName, 1).


generatePNGFromLattices([], _, _).
generatePNGFromLattices([Lattice|Lattices], FileName, Num) :-
	appendVulns(Lattice, ListOfVulns),
	append(ListOfVulns, Result),
	number_string(Num, NumString),
	formatDotVulns(Result, Str),
	format(atom(NewFileName), "~s~s.gv", [FileName, NumString]),
	generatePNGFromDot(Str, NewFileName), !,
	NewNum is Num+1,
	generatePNGFromLattices(Lattices, FileName, NewNum).

% Generates lattices in the directory where it belongs
% renamed from 'generateLatticeInDirectory'
generatePNGFromDotInDirectory([], _).
generatePNGFromDotInDirectory(Lattices, DirectoryName) :-
	generatePNGFromDotInDirectory(Lattices, DirectoryName, 1).

% renamed from 'generateLatticeInDirectory'
generatePNGFromDotInDirectory([], _, _).
generatePNGFromDotInDirectory([Lattice|Lattices], DirectoryName, Num) :-
	appendVulns(Lattice, ListOfVulns),
	append(ListOfVulns, Result),
	number_string(Num, NumString),
	formatDotVulns(Result, Str),
	format(atom(NewDirectoryName), "~s~s/lattice.gv", [DirectoryName, NumString]),
	generatePNGFromDot(Str, NewDirectoryName), !,
	NewNum is Num+1,
	generatePNGFromDotInDirectory(Lattices, DirectoryName, NewNum).

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
	format("LATTICES: ~w, COMPLEXITY: ~w, SUM: ~w, REST: ~w", [Lattices, Complexity, Sum, Rest]),
	calculateLatticeComplexity(Lattices, Rest),
	flush_output.

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
% Example: createAllLatticesFromIGS([server_access_root], [], Lattices), shortestPathInLattices(Lattices, Shortest)
shortestPathInLattices([], []).
shortestPathInLattices([Lattice|Lattices], [[Shortest]|Rest]) :-
	predsort(sortByLength, Lattice, SortedPaths),
	nth0(0, SortedPaths, Shortest),
    shortestPathInLattices(Lattices, Rest).

% Constrains results to a minimum length
% Example: createAllLatticesFromIGS([server_access_root], [], Lattices), filterLatticesByShortest(10, Lattices, Result)
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


% BROKEN: createAllLatticesFromIGS returns a list of paths, not just paths,
% since we are now grouping paths by their configs (i.e., making distinct lattices)
shortestPath(Goal, InitialState) :-
	createAllLatticesFromIGS(Goal, InitialState, createAllLatticesFromIGS),
	predsort(sortByLength, createAllLatticesFromIGS, SortedPaths),
	nth0(0, SortedPaths, (Configs, Vulns)),
	formatDotVulns(Vulns, Str),
	generatePNGFromDot(Str, 'shortestPath-test.gv'),
	createYamlFiles(Configs).

% BROKEN: createAllLatticesFromIGS returns a list of paths, not just paths,
% since we are now grouping paths by their configs (i.e., making distinct lattices)
longestPath(Goal, InitialState) :-
	createAllLatticesFromIGS(Goal, InitialState, createAllLatticesFromIGS),
	predsort(sortByLength, createAllLatticesFromIGS, SortedPaths),
	last(SortedPaths, (Configs, Vulns)),
	formatDotVulns(Vulns, Str),
	generatePNGFromDot(Str, 'longestPath-test.gv'),
	createYamlFiles(Configs).

getConfigs([], _, _, _).
getConfigs([Lattice|Lattices], Name, Num, Params) :-
	nth0(0, Lattice, First),
	getConfig(First, Name, Num, Params),
	NewNum is Num+1,
	getConfigs(Lattices, Name, NewNum, Params).

getConfig([], _, _, _).
getConfig((Configs, _), Name, Num, Params) :-
	createYamlFiles(Configs, Name, Num, Params).

createYamlFiles(Configs, Name, Num, Params) :-
	formatRoles(Configs, Roles),
	createPlaybook(Roles, Name, Num),
	number_string(Num, NumString),
	format(atom(DirectoryName), "~s~s/vars", [Name, NumString]),
	make_directory(DirectoryName),
	listRoles(Configs, Vars, Params),
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

listRoles([], "", _).
listRoles([Role-Val|Rest], String, Params) :-
	listKeys(Val, String1, Params),
	format(atom(Out), "~s:~n~s~n", [Role, String1]),
	listRoles(Rest, String2, Params),
	format(atom(String), "~s~s", [Out, String2]).

listKeys([], "", _).
listKeys([Key-(_, Vals)|Rest], String, Params) :-
	listVals(Vals, String1, Params),
	format(atom(Out), "~t~2|~s:~n~s", [Key, String1]),
	listKeys(Rest, String2, Params),
	format(atom(String), "~s~s", [Out, String2]).

listVals([], "", _).
listVals([Val|Vals], String, Params) :-
	listVals(Vals, String1, Params),
	format(atom(String), "~t~4|- ~s~n~s", [Val, String1]).
listVals(Predicate, String, Params) :-
	list_to_assoc(Params, Assoc),
	call(Predicate, Assoc, Output),
	format(atom(String), "~t~4|- ~s~n", [Output]).

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


replace_substring(String, To_Replace, Replace_With, Result) :-
    append([Front, To_Replace, Back], String),
    append([Front, Replace_With, Back], Result).

% MySQL Commands
replace_word(Old, New, Orig, Replaced) :-
    atomic_list_concat(Split, Old, Orig),
    atomic_list_concat(Split, New, Replaced). 

query(USER, PWD, DB, QUERY, Columns, Rows) :-
	atom_concat('-p', PWD, PPWD),
	process_create(path(mysql), ['-u', USER, PPWD, '-D', DB, '-e', QUERY], [stdout(pipe(Out)),stderr(std)]),
	read_record(Out, Columns),
	read_records(Out, Rows).

read_record(Out, Fields) :-
	read_line_to_codes(Out, Codes),
	Codes \= end_of_file,
	atom_codes(Line, Codes),
	replace_word('NULL', '[]', Line, R),
	atomic_list_concat(Fields, '\t', R).

read_records(Out, [Record|Rs]) :-
	read_record(Out, Record),
	!, read_records(Out, Rs).
read_records(Out, []) :-
	close(Out).

% convert MySQL table into ProLog knowledge base
capture_table(USER, PWD, DB, QUERY, Functor) :-
	query(USER, PWD, DB, QUERY, _Columns, Rows),
	maplist(capture_table(Functor), Rows).

capture_table(Functor, [Vuln|VulnProps]) :-
	maplist(term_string, VulnPropTerms, VulnProps),
	Clause =.. [Functor|[Vuln|VulnPropTerms]],
	assertz(Clause).

initialSetup(Username, Password) :-
	capture_table(Username, Password, "vuln", "select vuln_name, concat('[',group_concat(distinct statesPre.states_name),']') as pre,concat('[',group_concat(distinct statesPost.states_name),']') as post, vuln_config from vuln left join vuln_pre on vuln.vuln_id = vuln_pre.vuln_id left join states as statesPre on vuln_pre.states_id=statesPre.states_id left join vuln_post on vuln_post.vuln_id=vuln.vuln_id left join states as statesPost on vuln_post.states_id=statesPost.states_id group by vuln.vuln_id;", vuln),
	!.

% creates a lattice w/ complexity specified in the specific bounds.
createLatticeWithComplexityIGS(Goal, InitialState, Lower, Upper, Name, Params) :-
	findLatticeWithComplexityIGS(Goal, InitialState, Lower, Upper, Lattice),
	createLatticeDirectories(Name, 1, 1),
	generateLatticeInDirectory([Lattice], Name),
	getConfigs([Lattice], Name, 1, Params).

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
createLatticeWithVulnIGS(Goal, InitialState, Name, Vuln, Params) :-
	createAllLatticesFromIGS(Goal, InitialState, Lattices),
	checkVulnLatticesForVuln(Lattices, Vuln, Lattice),
	createLatticeDirectories(Name, 1, 1),
	generatePNGFromDotInDirectory([Lattice], Name),
	getConfigs([Lattice], Name, 1, Params).

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
generatePasswordOfLength(Params, Password) :-
	get_assoc(paramPasswordLength, Params, Length),
	passwords(Passwords),
	generateFromList(Passwords, Length, Output),
	atom_chars(GenPwd, Output),
	Password = GenPwd.

passwords(['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
	'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
	'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '1', '2', '3', '4',
	'5', '6', '7', '8', '9', '0', '!', '@', '#', '$', '%' ,'^', '&', '*', '(', ')']).

usernames(['admin', 'bbelna', 'jeckroth', 'guest']).
