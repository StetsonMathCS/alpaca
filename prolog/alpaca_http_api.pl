% run as: swipl -s alpaca_http_api.pl -g 'server(10333)'

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_unix_daemon)).
:- initialization(run, main).
:- http_handler('/alpaca', handle, [time_limit(30)]).
:- [alpaca].

run :- http_daemon([port(10333)]).

validPredicates([graphAllVulns, createRangeFromIGS,
                 createStartRangeFromIGS, createAllLatticesFromIGS]).

server(Port) :- http_server(http_dispatch, [port(Port)]).

read_term_from_atom([],[]). % need special case for empty list
read_term_from_atom(A,T) :- read_term_from_atom(A,T,[]).

term_to_dict([], [], Pairs, Dict) :-
    dict_pairs(Dict, _, Pairs).
term_to_dict([VarAtom|VarsAtoms], [Var|Vars], Pairs, Dict) :-
    term_string(Var, VarStr),
    term_to_dict(VarsAtoms, Vars, [VarAtom-VarStr|Pairs], Dict).

join_args([Arg], Arg).
join_args([Arg|Args], Out) :-
    join_args(Args, Rest),
    string_concat(Arg, ',', ArgComma),
    string_concat(ArgComma, Rest, Out), !.

extract_vnames([],[]).
extract_vnames([=(VName,_)|Rest], [VName|VNamesRest]) :-
    extract_vnames(Rest, VNamesRest).

vars_to_varnames(Args, VarNames) :-
    join_args(Args, Joined),
    term_string(_, Joined, [variable_names(VNames)]),
    extract_vnames(VNames, VarNames).

handle(Request) :-
    http_read_json(Request, JSONIn),
    json_to_prolog(JSONIn, [Pred|Args]),
    http_log("~p~n", [[Pred|Args]]),
    maplist(read_term_from_atom, Args, ArgsTerms),
    validPredicates(ValidPredicates),
    member(Pred, ValidPredicates),
    term_variables(ArgsTerms, Vars),
    vars_to_varnames(Args, VarNames),
    aggregate_all(set(Dict), (Goal =.. [Pred|ArgsTerms],
                              call(Goal),
                              term_to_dict(VarNames, Vars, [], Dict)),
                  Results),
    http_log("~p ---> ~p~n", [[Pred|Args], Results]),
    reply_json_dict(Results).

