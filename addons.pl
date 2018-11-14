
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
	allPaths(Goal, InitialState, Lattices),
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
	allPaths(Goal, InitialState, Lattices),
	checkVulnLatticesForVuln(Lattices, Vuln, Lattice),
	createLatticeDirectories(Name, 1, 1),
	generateLatticeInDirectory([Lattice], Name),
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
generatePasswordOfLength(Password, Length) :-
  passwords(Passwords),
	generateFromList(Passwords, Length, Output),
	atom_chars(GenPwd, Output),
	Password = GenPwd.
