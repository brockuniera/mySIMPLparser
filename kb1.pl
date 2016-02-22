pphrase(A, B, C) :- phrase(A, B, C), format('~s~n', [B]).

prog --> retStatement, [.].
prog --> declaration, [';'], prog.
prog --> assignment, [';'], prog.
assignment --> id, [':='], base.
declaration --> ['var'], id.
retStatement --> [return], base.
base --> id.
base --> num.
base --> ['('], expr, [')'].

% TODO Handle left recursion
expr(E) --> term(T), addOp(Op), left_assoc(E, T, Op).
left_assoc(n_expression(Op, T, T1), T, Op) --> term(T1).
left_assoc(E, T, Op) --> term(T1), addOp(Op), left_assoc(E, n_expression(Op, T, T1), Op).
%(- (- 1 2) 3) === ((1 - 2) - 3)

term(n_num(Y)) --> [X], {atom_number(X, Y)}.

%term --> factor.
factor --> base.

addOp(plus) --> [+].
addOp(minus) --> [-].
mulOp(times) --> [*].
mulOp(divide) --> [/].


% < id > definition
id -->
    [X],
    {
        \+ member(X, [return, 'var']),
        atom_codes(X, S), alphaword(S)
    }.
alphaword([]).
alphaword([H|T]) :- char_type(H, alpha), alphaword(T).

% < number > definition
num --> [X], {atom_number(X, _)}.
