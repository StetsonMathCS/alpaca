%run using: swipl -s alpaca_tests.pl -t run_tests

:- use_module(library(test_cover)).

:- [alpaca].
:- begin_tests(main).


test(graphAllVulns, [nondet]) :-
        graphAllVulns('Testfile').

%test(createRangeFromIGS) :-
%        createRangeFromIGS([server_access_root], [], server_access_root40).


test(add) :-
        A is 1 + 2,
        A =:= 3.

:- end_tests(main).

