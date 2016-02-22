:- consult(assignment2).

% Test some simple structures: Returning integers and floating points
test_simple_1 :- interpret(['return', 5, '.'], N), N =:= 5.
test_simple_2 :- interpret(['return', 14.6, '.'], N), N =:= 14.6.

test_simple :- write('Testing simple...\n'), test_simple_1, test_simple_2.

% Test setting variables and returning them.
test_var_1 :- interpret(['var', 'foo', ';',
                         'foo', ':=', 6, ';',
                         'return', 'foo', '.'], N), N =:= 6.
test_var_2 :- \+ interpret(['foo', ':=', 6, ';',
                            'return', 'foo', '.'], _).

test_var :- write('Testing var...\n'), test_var_1, test_var_2.

% Test order of operations and associativity
test_oop_1 :- interpret(['var', 'foo', ';', 'foo', ':=', '(', 10, '/', 2, ')', ';', 'return', 'foo', '.'], N), N =:= 5.
test_oop_2 :- interpret(['var', 'foo', ';', 'foo', ':=', '(', 10, '-', 2, ')', ';', 'return', 'foo', '.'], N), N =:= 8.
test_oop_3 :- interpret(['var', 'foo', ';', 'foo', ':=', '(', 10, '/', 2, '/', 5, ')', ';', 'return', 'foo', '.'], N), N =:= 1.
test_oop_4 :- interpret(['var', 'foo', ';', 'foo', ':=', '(', 10, '-', 2, '-', 5, ')', ';', 'return', 'foo', '.'], N), N =:= 3.
test_oop_5 :- interpret(['var', 'foo', ';', 'foo', ':=', '(', 10, '+', 2, '-', 3, ')', ';', 'return', 'foo', '.'], N), N =:= 9.
test_oop_6 :- interpret(['var', 'foo', ';', 'foo', ':=', '(', 10, '*', 2, '-', 3, ')', ';', 'return', 'foo', '.'], N), N =:= 17.

test_oop :- write('Testing operations and associativity...\n'), test_oop_1, test_oop_2, test_oop_3, test_oop_4, test_oop_5, test_oop_6.

% Some comprehensive tests
test_comp_1 :- interpret(['var', 'foo', ';',
                          'foo', ':=', '(', 1, '+', 2.5, ')', ';',
                          'return', '(', 'foo', '*', 5, ')', '.'], N), N =:= 17.5.
test_comp_2 :- interpret(['var', 'foo', ';', 'foo', ':=', '(', 1, '+', 2.5, ')', ';', 'return', 'foo', '.'], N), N =:= 3.5.
test_comp_3 :- interpret(['var', 'x', ';', 'x',':=', '(', 5, '*', 2, ')', ';', 'return', '(', 'x', '+', 1, ')', '.'], N), N =:= 11.
test_comp_4 :- interpret(['var', 'foo', ';', 
                          'var', 'bar', ';',
                          'var', 'lol', ';',
                          'foo', ':=', 1, ';',
                          'bar', ':=', 'foo', ';',
                          'lol', ':=', 'bar', ';',
                          'return', 'lol', '.'], N), N =:= 1.

test_comp :- write('Testing comprehensive...\n'), test_comp_1, test_comp_2, test_comp_3, test_comp_4.

% Check to make sure that we crash in certain illegal cases
test_reject_1 :- \+ parse(['return', 5], _).
test_reject_2 :- \+ parse(['return', 5, '+', 6, '.'], _).
test_reject_3 :- \+ parse(['var', 'foo', ';', 'foo', ':=', 5, '+', 6, '.'], _).
test_reject_4 :- \+ parse(['var', 'foo', ';',
                           'foo', ':=', '5', '+', '6', ';',
                           'return', 'foo', '.'], _).
test_reject_5 :- \+ parse(['var', 'var', ';', 'return', 5, '.'], _).
test_reject_6 :- \+ parse(['var', 'x', ';', 'return', 'var', '.'], _).
test_reject_7 :- \+ parse(['var', '+', ';', 'return', '+', '.'], _).
test_reject_8 :- \+ interpret(['var', 'x', ';', 'return', 'x', '.'], _).
test_reject_9 :- \+ interpret(['var', 'x', ';', 'y', ':=', 1, ';', 'return', 1, '.'], _).

test_reject :- write('Testing rejects...\n'), test_reject_1, test_reject_2, test_reject_3, test_reject_4, test_reject_5, test_reject_6, test_reject_7, test_reject_8, test_reject_9.

% Pull it all together....
test_all :- test_simple, test_var, test_oop, test_comp, test_reject, write('Success!\n').
