
:- use_module(library(uuid)).
:- use_module(library(http/json)).

rangesDir("../ranges").

outputRange(InitialState, Goal, Params, Lattices) :-
    uuid(RangeId, [version(4)]),
    format("Creating range ~s~n", [RangeId]),
    rangesDir(RangesDir),
    format(atom(RangeDir), "~s/~s", [RangesDir, RangeId]),
    make_directory_path(RangeDir),
    outputRangeMetadata(RangeDir, RangeId, InitialState, Goal, Params, Lattices),
    printLattices(Lattices),nl,
    maplist(outputLattice(RangeDir, RangeId), Lattices).

outputRangeMetadata(RangeDir, RangeId, InitialState, Goal, Params, Lattices) :-
    format(atom(RangeMetadataFname), "~s/range_metadata.json", [RangeDir]),
	open(RangeMetadataFname, write, Stream),
    jsonifyLattices(Lattices, JsonLattices),
    json_write(Stream, json([rangeId-RangeId, initialState-InitialState, goal-Goal,
                             params-json(Params), lattices-JsonLattices])),
    close(Stream).

jsonifyLattices([], []).
jsonifyLattices([(Config,Vulns)|Rest], [json([config-JsonConfig, vulns-JsonVulns])|JsonRest]) :-
    jsonifyConfig(Config, JsonConfig),
    jsonifyVulns(Vulns, JsonVulns),
    jsonifyLattices(Rest, JsonRest).

jsonifyConfig([], []).
jsonifyConfig([Key-Vals|Rest], [json([Key-json(JsonVals)])|JsonRest]) :-
    jsonifyConfigVals(Vals, JsonVals),
    jsonifyConfig(Rest, JsonRest).

jsonifyConfigVals([], []).
jsonifyConfigVals([Key-(_Quantifier,[V|Vals])|Rest], [Key-[V|Vals]|JsonRest]) :-
    !,
    jsonifyConfigVals(Rest, JsonRest).
jsonifyConfigVals([Key-(_Quantifier,V)|Rest], [Key-[V]|JsonRest]) :-
    jsonifyConfigVals(Rest, JsonRest).

jsonifyVulns([], []).
jsonifyVulns([(Input, Description, Output)|Rest], [Json|JsonRest]) :-
    Json = json([input-Input, description-Description, output-Output]),
    jsonifyVulns(Rest, JsonRest).

outputLattice(RangeDir, RangeId, Lattice) :-
    uuid(LatticeId, [version(4)]),
    format("Creating lattice ~s in range ~s~n", [LatticeId, RangeId]),
    format(atom(LatticeDir), "~s/~s", [RangeDir, LatticeId]),
    make_directory_path(LatticeDir),
    generatePNGFromLattice(LatticeDir, Lattice),
    Lattice = (Configs, _),
	createYamlFiles(Configs, LatticeDir), !.

createYamlFiles(Configs, LatticeDir) :-
    format(atom(AnsibleDir), "~s/ansible", [LatticeDir]),
    make_directory_path(AnsibleDir),
	formatRoles(Configs, Roles),
	createPlaybook(Roles, AnsibleDir),
	format(atom(VarsDir), "~s/ansible/vars", [LatticeDir]),
	make_directory_path(VarsDir),
	listRoles(Configs, Vars),
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
listVals(Val, String) :-
	format(atom(String), "~t~4|- ~s~n", [Val]).
