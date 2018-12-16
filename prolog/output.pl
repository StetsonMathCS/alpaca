
outputLattice(RangeId, Params, Lattice) :-
    uuid(LatticeId, [version(4)]),
    format("Creating lattice ~s in range ~s~n", [LatticeId, RangeId]),
    rangesDir(RangesDir),
    format(atom(LatticeDir), "~s/~s/~s", [RangesDir, RangeId, LatticeId]),
    make_directory_path(LatticeDir),
    generatePNGFromLattice(LatticeDir, Lattice),
    Lattice = (Configs, _),
	createYamlFiles(Configs, Params, LatticeDir), !.

createYamlFiles(Configs, Params, LatticeDir) :-
    format(atom(AnsibleDir), "~s/ansible", [LatticeDir]),
    make_directory_path(AnsibleDir),
	formatRoles(Configs, Roles),
	createPlaybook(Roles, AnsibleDir),
	format(atom(VarsDir), "~s/ansible/vars", [LatticeDir]),
	make_directory_path(VarsDir),
	listRoles(Configs, Vars, Params),
	createVars(Vars, VarsDir).

createPlaybook(Roles, AnsibleDir) :-
	format(atom(PlaybookFname), "~s/playbook.yml", [AnsibleDir]),
    format("Writing ~s~n", [PlaybookFname]),
	open(PlaybookFname, write, Stream),
	format(atom(String),
		"---~n- hosts: all~n~t~2|become: true~n~t~2|vars_files:~n~t~4|- vars/all.yml~n~t~2|roles:~n~s", [Roles]),
	write(Stream, String),
	close(Stream).

createVars(Vars, VarsDir) :-
	format(atom(VarsFname), "~s/all.yml", [VarsDir]),
    format("Writing ~s~n", [VarsFname]),
	open(VarsFname, write, Stream),
	format(atom(String), "---~n~s", [Vars]),
	write(Stream, String),
	close(Stream).

formatRoles([], "").
formatRoles([Role-_|Configs], String) :-
	formatRoles(Configs, String1),
	format(atom(String), "~t~4|- ~s~n~s", [Role, String1]).

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
