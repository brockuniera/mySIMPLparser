%base --> [X], {checkid(X)}.
base --> [X], {id(X)}.
id(X) :-
    \+ member(X, [return, 'var']),
    atom_codes(X, S), alphaword(S).

alphaword([]).
alphaword([H|T]) :- char_type(H, alpha), alphaword(T).

