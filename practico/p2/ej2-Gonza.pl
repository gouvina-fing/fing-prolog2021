largoA([],0).
largoA([_|L], X) :- largoA(L, Y), X is Y + 1.

maximoA([X], X).
maximoA([H|L],H) :- maximoA(L, N), H >= N.
maximoA([H|L],N) :- maximoA(L, N), N > H.

% mismos predicados pero con acumuladores.
% ---------------------------------------------------------------
largoB([],L,L).
largoB([_|L], L1, R) :- L2 is L1 + 1, largoB(L, L2, R).

maximoB([], M, M).
maximoB([H|L], M, R) :- H >= M, maximoB(L, H, R).
maximoB([H|L], M, R) :- M > H, maximoB(L, M, R).
