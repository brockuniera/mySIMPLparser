writeln(A) :- write(A), write('\n').

parse(TokenList, AST) :- phrase(prog(AST), TokenList).
evaluate(AST, Number) :- empty_assoc(Min), ev(AST, Number, Min, _).

interpret(TokenList, Number) :- parse(TokenList, AST), evaluate(AST, Number).

% Just eat progs, returns, bases, etc
ev(prog(return(A)), N, Min, Mout) :- ev(A, N, Min, Mout).
ev(prog(A, B), N, Min, Mout) :- ev(A, N, Min, M1), ev(B, N, M1, Mout).
ev(base(I), N, Min, Mout) :- ev(I, N, Min, Mout).
ev(expr(T), N, Min, Mout) :- ev(T, N, Min, Mout).
ev(term(T), N, Min, Mout) :- ev(T, N, Min, Mout).
ev(factor(T), N, Min, Mout) :- ev(T, N, Min, Mout).

% Terms
ev(term(times, T, T1), N, Min, Mout) :- ev(T, N1, Min, M1), ev(T1, N2, M1, Mout), N is N1 * N2.
ev(term(divide, T, T1), N, Min, Mout) :- ev(T, N1, Min, M1), ev(T1, N2, M1, Mout), N is N1 / N2.

% Expressions
ev(expr(plus , T, T1), N, Min, Mout) :- ev(T, N1, Min, M1), ev(T1, N2, M1, Mout), N is N1 + N2.
ev(expr(minus, T, T1), N, Min, Mout) :- ev(T, N1, Min, M1), ev(T1, N2, M1, Mout), N is N1 - N2.

% Numbers, ops allowed.
ev(num(N), N, M, M).
ev(plus(_), _, M, M).
ev(minus(_), _, M, M).
ev(times(_), _, M, M).
ev(divide(_), _, M, M).

% Declarations. id cannot have existed before, and now it does. Mout != Min.
ev(declr(id(I)), _, Min, Mout) :- \+ get_assoc(I, Min, _), put_assoc(I, Min, unassigned, Mout).

% Assignments are valid when LHS id is in Min, and base is valid. Mout != Min.
ev(assn(id(I), base(B)), _, Min, Mout) :- ev(B, N, Min, _), get_assoc(I, Min, _), put_assoc(I, Min, assigned(N), Mout).

% Reading an id. The id has to exist and be assigned in Min.
ev(id(I), N, Min, _) :- get_assoc(I, Min, assigned(N)).


prog(prog(R)) --> retStatement(R), [.].
prog(prog(D, P)) --> declaration(D), [';'], prog(P).
prog(prog(A, P)) --> assignment(A), [';'], prog(P).
declaration(declr(I)) --> ['var'], id(I).
assignment(assn(I, B)) --> id(I), [':='], base(B).
retStatement(return(B)) --> [return], base(B).
base(base(I)) --> id(I).
base(base(N)) --> num(N).
base(base(E)) --> ['('], expr(E), [')'].

expr(expr(T)) --> term(T).
expr(E) --> term(T), addOp(Op), left_assoc(E, T, Op).
left_assoc(expr(Op, T, T1), T, Op) --> term(T1).
left_assoc(E, T, OpTtoT1) --> term(T1), addOp(Op), left_assoc(E, expr(OpTtoT1, T, T1), Op).
%(+ (- 1 2) 3) === ((1 - 2) + 3) === 1 - 2 + 3

term(term(F)) --> factor(F).
term(T) --> factor(F), mulOp(Op), left_assoc_t(T, F, Op).
left_assoc_t(term(Op, F, F1), F, Op) --> factor(F1).
left_assoc_t(T, F, OpFtoF1) --> factor(F1), mulOp(Op), left_assoc_t(T, term(OpFtoF1, F, F1), Op).

factor(factor(B)) --> base(B).

addOp(plus) --> [+].
addOp(minus) --> [-].
mulOp(times) --> [*].
mulOp(divide) --> [/].

% < id > definition
id(id(I)) --> [I], { \+ member(I, [return, 'var']), atom_codes(I, S), alphaword(S) }.
alphaword([]).
alphaword([H|T]) :- char_type(H, alpha), alphaword(T).

% < number > definition
num(num(X)) --> [X], {number(X)}.
