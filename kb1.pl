pphrase(A, B, C) :- phrase(A, B, C), format('~s~n', [B]).

prog --> retStatement, [.].
prog --> declaration, [';'], prog.
%prog --> assignment, [';'], prog.
retStatement --> [return], base.
base --> [X], {id(X)}.
