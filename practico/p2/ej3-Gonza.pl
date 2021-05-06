suma([], 0).
suma([_|L], N) :- suma(L, N1), N is N1 + 1.

pares([],[]).
pares([H|L],[H|L1]) :- 0 is mod(H,2), pares(L, L1).
pares([H|L],L1) :- 1 is mod(H,2), pares(L, L1).

mayores([],_,[]).
mayores([H|L], X, [X|LM]) :- H > X, mayores(L, X, LM).
mayores([H|L], X, LM) :- not(H > X), mayores(L, X, LM). 


merge([],L,L).
merge(L,[],L).
merge([H1|L1], [H2|L2], [H1|L3]) :- H1 =< H2, merge(L1, [H2|L2], L3).
merge([H1|L1], [H2|L2], [H2|L3]) :- H2 < H1, merge([H1|L1], L2, L3).
    