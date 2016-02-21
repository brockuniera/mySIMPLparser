pphrase(X, Y, Z) :- phrase(X, Y, Z), format('~s~n', [Y]).
cliche -->
    thing,
    " is a ", 
    type_of_thing, 
    " trapped in a ", 
    opposite_type_of_thing, 
    " body.".
thing --> "Cygwin".
thing --> "ayy".
type_of_thing --> "Unix OS".
opposite_type_of_thing --> "Windows'".
