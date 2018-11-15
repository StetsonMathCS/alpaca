:- [vulnDatabase]. %import vulnDatabase.pl
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

% Finds all lattices, create directories, generate lattices in directory, create ansible playbooks
% renamed from 'createRange'
createRangeFromIGS(Goal, InitialState, DirectoryName) :-
    createAllLatticesFromIGS(Goal, InitialState, Lattices),
    length(Lattices, Length),
    createLatticeDirectories(DirectoryName, 1, Length),
    generatePNGFromDotInDirectory(Lattices, DirectoryName),
    getConfigs(Lattices, DirectoryName, 1),
    format(atom(NewDirectoryName), "~s~s", [DirectoryName, "1"]),
    open('ansible/playbook.yml', write, Stream),
    format(atom(String), "---~n- import_playbook: ../~s/playbook.yml", [NewDirectoryName]),
    write(Stream, String),
    close(Stream).

% Initializes the range as a virtual machine
% Example: createStartRangeFromIGS(['server_access_root'], [], 'server_access_root')
% renamed from 'createAllPaths'
createStartRangeFromIGS(VMname) :-
    format(atom(Command1), "vagrant up ~s", [VMname]),
    format(atom(Command2), "vagrant halt ~s", [VMname]),
    shell(Command1),
    shell(Command2).
	%shell('vagrant up sr_range_1'),
    %shell('vagrant halt sr_range_1').

initiateDevFile :-
    open('ansible/inventories/dev2', write, Stream),
    write(Stream, '[test-web]'), nl(Stream),
    close(Stream).

addAddrToDevFile :-
    append('ansible/inventories/dev2'),
    write('Here is another line'), nl, told.

write_list([]).
write_list([H|T]):-
    write(H), nl,
    write_list(T).

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
