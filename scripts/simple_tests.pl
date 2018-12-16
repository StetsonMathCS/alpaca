% run as: swipl --traditional -s simple_tests.pl -t run_tests

:- use_module(library(test_cover)).

:- begin_tests(simple).

test(add) :-
	A is 1+2,
	A =:= 3.

test(subtract) :-
	A is 5-3,
	A =:= 2.

test(multiply) :-
	A is 5*3,
	A =:= 15.

test(divide) :-
	A is 20/2,
	A =:= 10.

%test(fail_add) :-
%	A is 1+2,
%	A =:= 5.
%
%test(fail_subtract) :-
%	A is 5-3,
%	A =:= 3.
%
%test(fail_multiply) :-
%	A is 5*3,
%	A =:= 10.
%
%test(fail_divide) :-
%	A is 20/2,
%	A =:= 20.
%
:- end_tests(simple).
