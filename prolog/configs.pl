
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

realizeConfigFromParams([], _, []).
realizeConfigFromParams([Key-Vals|ConfigRest], Params, [Key-ValsRealized|ConfigRealizedRest]) :-
    realizeKeysValsFromParams(Vals, Params, ValsRealized),
    realizeConfigFromParams(ConfigRest, Params, ConfigRealizedRest).

realizeKeysValsFromParams([], _, []).
realizeKeysValsFromParams([Key-(Quantifier,Vals)|KeysValsRest], Params, [Key-(Quantifier,ValsRealized)|KeysValsRealizedRest]) :-
    realizeValsFromParams(Vals, Params, ValsRealized),
    realizeKeysValsFromParams(KeysValsRest, Params, KeysValsRealizedRest).

realizeValsFromParams([], _, []).
realizeValsFromParams([Val|ValsRest], Params, [ValRealized|ValsRealizedRest]) :-
    (string(Val) ->
        ValRealized = Val ;
        list_to_assoc(Params, Assoc),
        call(Val, Assoc, ValRealized)),
    realizeValsFromParams(ValsRest, Params, ValsRealizedRest).

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
	nth0(0, Output, UsernameAtom),
    atom_string(UsernameAtom, Username).

% generates a password, pulling letters from a dictionary
generatePassword(Params, Password) :-
	get_assoc(paramPasswordLength, Params, Length),
	passwords(Passwords),
	generateFromList(Passwords, Length, Output),
	string_chars(Password, Output).

passwords(['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
	'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
	'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '1', '2', '3', '4',
	'5', '6', '7', '8', '9', '0', '!', '@', '#', '$', '%' ,'^', '&', '*', '(', ')']).

usernames(['admin', 'jane', 'john', 'guest']).

