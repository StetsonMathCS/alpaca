
:- use_module(library(uuid)).
:- use_module(library(http/json)).

outputRange(InitialState, Goal, Params, Lattices) :-
    uuid(RangeId, [version(4)]),
    format("Creating range ~s~n", [RangeId]),
    format(atom(RangeDirRel), "../ranges/~s", [RangeId]),
    absolute_file_name(RangeDirRel, RangeDir),
    make_directory_path(RangeDir),
    findall((LatticeId, Lattice), (member(Lattice, Lattices), uuid(LatticeId, [version(4)])), LatticesWithIds),
    outputRangeMetadata(RangeDir, RangeId, InitialState, Goal, Params, LatticesWithIds),
    printLattices(LatticesWithIds), nl,
    maplist(outputLattice(RangeDir, RangeId), LatticesWithIds),
    working_directory(OrigWD, "../ranges"),
    format(atom(Command), "zip -r ~s.zip ~s", [RangeId, RangeId]),
    shell(Command),
    working_directory(_, OrigWD).

outputRangeMetadata(RangeDir, RangeId, InitialState, Goal, Params, LatticesWithIds) :-
    format(atom(RangeMetadataFname), "~s/range_metadata.json", [RangeDir]),
	open(RangeMetadataFname, write, Stream),
    jsonifyLattices(LatticesWithIds, JsonLattices),
    json_write(Stream, json([rangeId-RangeId, initialState-InitialState, goal-Goal,
                             params-json(Params), lattices-JsonLattices])),
    close(Stream).

jsonifyLattices([], []).
jsonifyLattices([(LatticeId, (Config,Vulns))|Rest], [json([latticeId-LatticeId, config-JsonConfig, vulns-JsonVulns])|JsonRest]) :-
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

outputLattice(RangeDir, RangeId, (LatticeId, Lattice)) :-
    format("Creating lattice ~s in range ~s~n", [LatticeId, RangeId]),
    format(atom(LatticeDir), "~s/~s", [RangeDir, LatticeId]),
    make_directory_path(LatticeDir),
    % link packer files
    format(atom(LatticePackerDir), "~s/packer", [LatticeDir]),
    absolute_file_name("../packer", PackerDir),
    link_file(PackerDir, LatticePackerDir, symbolic),
    format(atom(LatticePackerScript), "~s/run_packer.sh", [LatticeDir]),
    absolute_file_name("../run_packer.sh", PackerScript),
    link_file(PackerScript, LatticePackerScript, symbolic),
    generatePNGFromLattice(LatticeDir, Lattice),
    Lattice = (Configs, _),
	createYamlFiles(Configs, LatticeDir), !.

createYamlFiles(Configs, LatticeDir) :-
    format(atom(AnsibleDir), "~s/roles", [LatticeDir]),
    absolute_file_name("../ansible/roles", ParentAnsibleDir),
    link_file(ParentAnsibleDir, AnsibleDir, symbolic),
    print(ParentAnsibleDir),nl,
	formatRoles(Configs, Roles),
	formatVars(Configs, Vars),
	createPlaybook(Vars, Roles, LatticeDir).

createPlaybook(Vars, Roles, LatticeDir) :-
	format(atom(PlaybookFname), "~s/playbook.yml", [LatticeDir]),
    format("Writing ~s~n", [PlaybookFname]),
	open(PlaybookFname, write, Stream),
	format(atom(String),
		"---~n- hosts: all~n~t~2|become: true~n~t~2|vars:~n~s~n~t~2|roles:~n~s", [Vars, Roles]),
	write(Stream, String),
	close(Stream).

formatRoles([], "").
formatRoles([Role-_|Configs], String) :-
	formatRoles(Configs, String1),
	format(atom(String), "~t~4|- ~s~n~s", [Role, String1]).

formatVars([], "").
formatVars([Role-Val|Rest], String) :-
	listKeys(Val, String1),
	format(atom(Out), "~t~6|~s:~n~s~n", [Role, String1]),
	formatVars(Rest, String2),
	format(atom(String), "~s~s", [Out, String2]).

listKeys([], "").
listKeys([Key-(_, Vals)|Rest], String) :-
	listVals(Vals, String1),
	format(atom(Out), "~t~8|~s:~n~s", [Key, String1]),
	listKeys(Rest, String2),
	format(atom(String), "~s~s", [Out, String2]).

listVals([], "").
listVals([Val|Vals], String) :-
	listVals(Vals, String1),
	format(atom(String), "~t~10|- ~s~n~s", [Val, String1]).
listVals(Val, String) :-
	format(atom(String), "~t~10|- ~s~n", [Val]).

% swipl repl helpers
printLattices([]).
printLattices([(LatticeId, (Config, Vulns))|Rest]) :-
    print('----'), nl,
    print(LatticeId), nl,
    print('Config: '), print(Config), nl,
    printVulns(Vulns), nl,
    printLattices(Rest).

printVulns([]).
printVulns([Vulns|Rest]) :-
	print(Vulns), nl,
	printVulns(Rest).

