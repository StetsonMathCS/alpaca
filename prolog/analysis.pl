
% creates a lattice w/ complexity specified in the specific bounds.
createLatticeWithComplexityIGS(InitialState, Goal, Lower, Upper, Name, Params) :-
	findLatticeWithComplexityIGS(InitialState, Goal, Lower, Upper, Lattice),
	createLatticeDirectories(Name, 1, 1),
	generateLatticeInDirectory([Lattice], Name),
	getConfigs([Lattice], Name, 1, Params).

findLatticeWithComplexityIGS(InitialState, Goal, Lower, Upper, Lattice) :-
	createAllLatticesFromIGS(InitialState, Goal, Lattices),
	calculateLatticeComplexity(Lattices, Sums),
	matchBoundedConstraint(Sums, Lower, Upper, Elem),
	indexOf(Sums, Elem, Index),
	nth0(Index, Lattices, Lattice).


% create a vulnerability lattice, constraining for
% a specific Vulnerability
createLatticeWithVulnIGS(InitialState, Goal, Name, Vuln, Params) :-
	createAllLatticesFromIGS(InitialState, Goal, Lattices),
	checkVulnLatticesForVuln(Lattices, Vuln, Lattice),
	createLatticeDirectories(Name, 1, 1),
	generatePNGFromDotInDirectory([Lattice], Name),
	getConfigs([Lattice], Name, 1, Params).

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
% Example: createAllLatticesFromIGS([], [server_access_root], Lattices), shortestPathInLattices(Lattices, Shortest)
shortestPathInLattices([], []).
shortestPathInLattices([Lattice|Lattices], [[Shortest]|Rest]) :-
	predsort(sortByLength, Lattice, SortedPaths),
	nth0(0, SortedPaths, Shortest),
    shortestPathInLattices(Lattices, Rest).

% Constrains results to a minimum length
% Example: createAllLatticesFromIGS([], [server_access_root], Lattices), filterLatticesByShortest(10, Lattices, Result)
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
shortestPath(InitialState, Goal) :-
	createAllLatticesFromIGS(InitialState, Goal, Lattices),
	predsort(sortByLength, Lattices, SortedPaths),
	nth0(0, SortedPaths, (Configs, Vulns)),
	formatDotVulns(Vulns, Str),
	generatePNGFromDot(Str, 'shortestPath-test.gv'),
	createYamlFiles(Configs).

% BROKEN: createAllLatticesFromIGS returns a list of paths, not just paths,
% since we are now grouping paths by their configs (i.e., making distinct lattices)
longestPath(InitialState, Goal) :-
	createAllLatticesFromIGS(InitialState, Goal, Lattices),
	predsort(sortByLength, Lattices, SortedPaths),
	last(SortedPaths, (Configs, Vulns)),
	formatDotVulns(Vulns, Str),
	generatePNGFromDot(Str, 'longestPath-test.gv'),
	createYamlFiles(Configs).

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

checkVulnLatticesForVuln([Lattice|_], Vuln, LatticeR) :-
	checkVulnAndConfig(Lattice, Vuln),
	LatticeR = Lattice.
checkVulnLatticesForVuln([_|Rest], Vuln, LatticeR) :-
	checkVulnLatticesForVuln(Rest, Vuln, LatticeR).

checkVulnAndConfig([(_, Vulns)|_], Vuln) :-
	checkVulns(Vulns, Vuln).
checkVulnAndConfig([_|Rest], Vuln) :-
	checkVulnAndConfig(Rest, Vuln).

%checkVulns([Vulns|_], Vuln) :-
%	with_output_to(atom(Ato), write(Vulns)),
%	sub_atom(Ato, B, L, A, Vuln).
%checkVulns([_|Rest], Vuln) :-
%	checkVulns(Rest, Vuln).

