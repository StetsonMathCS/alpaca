:- initialization main.
:- [alpaca].

/**
 * Run as:  swipl main.pl <predicateName> <args>
 * 
 * examples:
 *      graphAllVulns (no arguments required -- swipl main.pl graphAllVulns)
 *      createStartRangeFromIGS (no arguments required currently because it only produces one VM)
 *      createRangeFromIGS '[Goal]' '[InitialState]' 'DirectoryName'
 * 
 * To measure performance of predicates, just put time in front of the predicateName
 *      swipl main.pl time <predicateName> <args>
 */

main :-
    current_prolog_flag(argv, Argv),
    parseArgs(Argv),
    halt(0).
main :-
    halt(1).

% Measure performace of predicate
parseArgs([Time|Rest]) :-
    Time = 'time',
    performance(Rest).
% Used for graphAllVulns
parseArgs([Pred]) :- 
    current_predicate(Pred/0),
    Run =.. [Pred], 
    call(Run).
% Used for createRangeFromIGS
parseArgs([Pred, Name]) :-
    atom_string(DirectoryName, Name),
    current_predicate(Pred/1),
    Run =.. [Pred, DirectoryName],
    call(Run).
% Used for createStartRangeFromIGS
parseArgs([Pred|Rest]) :-
    argsToTerm(Rest, Goal, Initial, Name),
    current_predicate(Pred/3),
    Run =.. [Pred, Goal, Initial, Name],
    call(Run).
    
% Parse args for createStartRangeFromIGS
argsToTerm([ArgsGoal, ArgsInitial, ArgsName], Goal, Initial, Name) :-
    term_to_atom(Goal, ArgsGoal),
    term_to_atom(Initial, ArgsInitial),
    atom_string(Name, ArgsName).

% Measure performance of finding all possible paths
performance([Pred]) :-
    current_predicate(Pred/0),
    Run =.. [Pred], 
    time(call(Run)).
% Used for createRangeFromIGS
performance([Pred, Name]) :-
    atom_string(DirectoryName, Name),
    current_predicate(Pred/1),
    Run =.. [Pred, DirectoryName],
    time(call(Run)).
% Used for createStartRangeFromIGS
performance([Pred|Rest]) :-
    argsToTerm(Rest, Goal, Initial, Name),
    current_predicate(Pred/3),
    Run =.. [Pred, Goal, Initial, Name],
    time(call(Run)).

/*
parseArgs([Pred|Rest]) :-
    argsToTerm(Rest, Goal, Initial),
    current_predicate(Pred/2),
    Run =.. [Pred, Goal, Initial],
    call(Run).

argsToTerm([Goal, Initial], TGoal, TInitial) :-
    term_to_atom(TGoal, Goal),
    term_to_atom(TInitial, Initial).
*/
