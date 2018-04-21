:- initialization main.
:- [alpaca].

/**
 * Run as:  swipl main.pl <predicateName> <args>
 * 
 * examples:
 *      allPossiblePaths (no arguments required -- swipl main.pl allPossible Paths)
 *      createAllPaths '[Goal]' '[InitialState]' 'Name'
 *      createRange 'Name'
 */

main :-
    current_prolog_flag(argv, Argv),
    parseArgs(Argv),
    halt(0).
main :-
    halt(1).

% Used for allPossiblePaths
parseArgs([Pred]) :- 
    current_predicate(Pred/0),
    Run =.. [Pred], 
    call(Run).
% Used for createRange
parseArgs([Pred, Name]) :-
    atom_string(DirectoryName, Name),
    current_predicate(Pred/1),
    Run =.. [Pred, DirectoryName],
    call(Run).
% Used for createAllPaths
parseArgs([Pred|Rest]) :-
    argsToTerm(Rest, Goal, Initial, Name),
    current_predicate(Pred/3),
    Run =.. [Pred, Goal, Initial, Name],
    call(Run).
    
% Parse args for createAllPaths
argsToTerm([ArgsGoal, ArgsInitial, ArgsName], Goal, Initial, Name) :-
    term_to_atom(Goal, ArgsGoal),
    term_to_atom(Initial, ArgsInitial),
    atom_string(Name, ArgsName).

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