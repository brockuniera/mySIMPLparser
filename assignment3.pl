

% for assignment2tests.pl
interpret(TokenList, Number) :- parse(TokenList, AST), evaluate(AST, Number).

% short versions of parse/2 and evaluate/2
p(T, A) :- parse(T, A).

empty_scope(_{vscope:_{}, fscope:_{}, staticscope:_{}, parent:[]}).

% +Scopein  Current scope
% -Scopeout New scope, with Scopein as a parent
new_scope(Scopein, Scopeout) :-
    copy_term(Scopein.staticscope, SS),
    empty_scope(NS),
    Scopeout = NS.put(staticscope, SS).put(parent, Scopein).

evaluate(AST, Number) :- e2(AST, Number, _).
e2(AST, Number, V)    :- empty_scope(S), ev(AST, Number, S, V), number(Number).

% ev/4
% +AST  A valid abstract syntax tree from parse.
% -N    A number, the resulting value from evaluating this piece of the AST.
%       ie return(num(3)) will have N is 3.
% +Ein  The scope before evaluating AST.
% -Eout The scope after evaluating AST.

% Just eat progs, returns, bases, etc. We want what's inside.
ev(prog(return(A)), N, Ein, Eout) :- ev(A, N, Ein, Eout).
ev(prog(A, B), N, Ein, Eout) :- ev(A, N, Ein, M1), ev(B, N, M1, Eout).
ev(base(I), N, Ein, Eout) :- ev(I, N, Ein, Eout).
ev(expr(T), N, Ein, Eout) :- ev(T, N, Ein, Eout).
ev(term(T), N, Ein, Eout) :- ev(T, N, Ein, Eout).
ev(factor(T), N, Ein, Eout) :- ev(T, N, Ein, Eout).
ev(stmt(S), N, Ein, Eout) :- ev(S, N, Ein, Eout).

% Function declaration. Scopeout will contain an extra definition for function
ev(func(id(Iname), id(Argid), Prog), _, Scopein, Scopeout) :-
    \+ (Scopein.fscope.get(Iname) = _),
    copy_term(Scopein.staticscope, SS),
    Scopeout = Scopein.put(fscope/Iname, tup(Argid, SS, Prog)).put(staticscope/Iname, _).

% Function call. Makes a new scope and evals the stored program.
ev(fcall(id(Iname), B), N, Scopein, Scopeout) :-
    ev(B, Num, Scopein, S1),

    get_fscope(Iname, S1, tup(Argid, Staticscope, Progrn)), % Get information from it

    new_scope(S1, NewScope),
    put_vscope(Argid, NewScope.put(staticscope, Staticscope), assigned(Num), NS1),

    ev(Progrn, N, NS1, NS2),

    Scopeout = NS2.parent.

% If statement
ev(if(Cond, St, Sf), _, Ein, Eout) :-
    new_scope(Ein, Newscope),
    ev(Cond, NC, Newscope, NS1),
    ( NC -> 
        ev(St, _, NS1, E1) ;
        ev(Sf, _, NS1, E1)
    ),
    Eout = E1.parent.

% Loops
% TODO Scoping
ev(while(Cond, _), _, Ein, Eout) :- ev(Cond, NC, Ein, Eout), \+ NC.

ev(while(Cond, St), _, Ein, Eout) :-
    new_scope(Ein, Newscope),
    ev(Cond, NC, Newscope, NS1),
    NC,
    ev(St, _, NS1, E1),
    ev(while(Cond, St), _, E1.parent, Eout).

% Statement sequences
ev(stmntseq(S), _, Ein, Eout) :- ev(S, _, Ein, Eout).
ev(stmntseq(S, Snext), _, Ein, Eout) :- ev(S, _, Ein, E1), ev(Snext, _, E1, Eout).

% Comparisons
ev(cond(eq, B, B1), N, Ein, Eout) :- ev(B, N1, Ein, M1), ev(B1, N2, M1, Eout), N = (N1 =:= N2).
ev(cond(lt, B, B1), N, Ein, Eout) :- ev(B, N1, Ein, M1), ev(B1, N2, M1, Eout), N = (N1 < N2).
ev(cond(gt, B, B1), N, Ein, Eout) :- ev(B, N1, Ein, M1), ev(B1, N2, M1, Eout), N = (N1 > N2).
ev(cond(le, B, B1), N, Ein, Eout) :- ev(B, N1, Ein, M1), ev(B1, N2, M1, Eout), N = (N1 =< N2).
ev(cond(ge, B, B1), N, Ein, Eout) :- ev(B, N1, Ein, M1), ev(B1, N2, M1, Eout), N = (N1 >= N2).
ev(cond(ne, B, B1), N, Ein, Eout) :- ev(B, N1, Ein, M1), ev(B1, N2, M1, Eout), N = (N1 =\= N2).

% Terms
ev(term(times, T, T1), N, Ein, Eout) :- ev(T, N1, Ein, M1), ev(T1, N2, M1, Eout), N is N1 * N2.
ev(term(divide, T, T1), N, Ein, Eout) :- ev(T, N1, Ein, M1), ev(T1, N2, M1, Eout), N is N1 / N2.

% Expressions
ev(expr(plus , T, T1), N, Ein, Eout) :- ev(T, N1, Ein, M1), ev(T1, N2, M1, Eout), N is N1 + N2.
ev(expr(minus, T, T1), N, Ein, Eout) :- ev(T, N1, Ein, M1), ev(T1, N2, M1, Eout), N is N1 - N2.

% Numbers allowed.
ev(num(N), N, M, M).

% Declarations. id cannot have existed before at this scope level, and now it does. Eout != Ein.
ev(declr(id(I)), _, Ein, Eout) :-
    \+ (Ein.vscope.get(I) = _),
    Eout = Ein.put(staticscope/I, _).put(vscope/I, unassigned).

% Assignments are valid when id I is in Ein, and base is valid.
ev(assn(id(I), base(B)), _, Ein, Eout) :- ev(B, N, Ein, E1), get_vscope(I, E1, _), put_vscope(I, E1, assigned(N), Eout).

% Reading an id. The id has to exist and be assigned in Ein.
ev(id(I), N, Ein, Ein) :- get_vscope(I, Ein, Num), !, assigned(N) = Num.

get_fscope(Key, Scopein, Value) :-
    Scopein.staticscope.get(Key) = _,
    ((Scopein.fscope.get(Key) = Value, !) ; get_fscope(Key, Scopein.parent, Value)).

get_vscope(Key, Scopein, Value) :-
    Scopein.staticscope.get(Key) = _,
    ((Scopein.vscope.get(Key) = Value, !) ; get_vscope(Key, Scopein.parent, Value)).

% Key doesn't exist
put_vscope(Key, Sin, V, Sout) :-
    \+ (Sin.staticscope.get(Key) = _),
    Sout = Sin.put(vscope/Key, V).put(staticscope/Key, _).

% Key exists at this level
put_vscope(Key, Sin, V, Sout) :-
    Sin.staticscope.get(Key) = _,
    Sin.vscope.get(Key) = _,
    Sout = Sin.put(vscope/Key, V).put(staticscope/Key, _).

% Key exists above this level
put_vscope(Key, Sin, V, Sout) :-
    Sin.staticscope.get(Key) = _,
    \+ (Sin.vscope.get(Key) = _),
    put_vscope(Key, Sin.parent, V, Out),
    Sout = Sin.put(parent, Out).

% Key doesn't exist
put_fscope(Key, Sin, V, Sout) :-
    \+ (Sin.staticscope.get(Key) = _),
    Sout = Sin.put(fscope/Key, V).put(staticscope/Key, _).

% Key exists at this level
put_fscope(Key, Sin, V, Sout) :-
    Sin.staticscope.get(Key) = _,
    Sin.fscope.get(Key) = _,
    Sout = Sin.put(fscope/Key, V).put(staticscope/Key, _).

% Key exists above this level
put_fscope(Key, Sin, V, Sout) :-
    Sin.staticscope.get(Key) = _,
    \+ (Sin.fscope.get(Key) = _),
    put_fscope(Key, Sin.parent, V, Out),
    Sout = Sin.put(parent, Out).

%
% Parser
%

parse(TokenList, AST) :- phrase(prog(AST), TokenList).

prog(prog(R)) --> retStatement(R), [.].
prog(prog(F, P)) --> funcDecl(F), [';'], prog(P).
prog(prog(S, P)) --> statement(S), [';'], prog(P).

funcDecl(func(Iname, Iarg, Prog)) --> [function], id(Iname), ['('], id(Iarg), [')'], ['{'], prog(Prog), ['}'].

retStatement(return(B)) --> [return], base(B).

statement(stmt(S)) --> declaration(S) ; assignment(S) ; conditional(S) ; loop(S).

declaration(declr(I)) --> ['var'], id(I).
assignment(assn(I, B)) --> id(I), [':='], base(B).

loop(while(Cond, S)) -->
    [while], ['('], condition(Cond), [')'], [do], statementSeq(S), [done].

conditional(if(Cond, St, Sf)) -->
    [if], ['('], condition(Cond), [')'], [then], statementSeq(St), [else], statementSeq(Sf), [endif].

statementSeq(stmntseq(S)) --> statement(S), [.].
statementSeq(stmntseq(S, Snext)) --> statement(S), [';'], statementSeq(Snext).

condition(cond(Op, LHS, RHS)) --> base(LHS), comp(Op), base(RHS).

base(base(I)) --> id(I).
base(base(N)) --> num(N).
base(base(E)) --> ['('], expr(E), [')'].
base(base(F)) --> funcCall(F).

funcCall(fcall(I, B)) --> id(I), ['('], base(B), [')'].

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

comp(eq) --> ['=='].
comp(lt) --> ['<'].
comp(gt) --> ['>'].
comp(le) --> ['<='].
comp(ge) --> ['>='].
comp(ne) --> ['!='].

% < id > definition
id(id(I)) --> [I], { \+ member(I, [function, return, 'var', if, then, else, endif, while, do, done]), atom_codes(I, S), aword(S) }.
aword([H|T]) :- char_type(H, csymf), alphaword(T).
alphaword([]).
alphaword([H|T]) :- char_type(H, csym), alphaword(T).

% < number > definition
num(num(N)) --> [X], {catch((atom_codes(X, S), number_codes(N, S)), _, (fail))}.
