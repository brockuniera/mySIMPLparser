pphrase(A, B, C) :- phrase(A, B, C), format('~s~n', [B]).

prog --> retStatement, [.].
prog --> declaration, [';'], prog.
%prog --> assignment, [';'], prog.
declaration --> ['var'], id.
retStatement --> [return], base.
base --> id.
base --> num.
%base --> ['('], expr, [')'].

% TODO Handle left recursion
expr --> term.
%expr --> term.
term --> factor.
%term --> factor.
%
factor --> base.
addOp --> [+].
addOp --> [-].
mulOp --> [*].
mulOp --> [/].


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
