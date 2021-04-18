% --------------------------
% Ejercicio 2
% --------------------------

% PARTE A
% --------------------------

largo([], 0).
largo([_|T], N) :- largo(T, N2), N is N2 + 1.

maximo([H], H).
maximo([H|T], H) :- maximo(T, M), H > M.
maximo([H|T], M) :- maximo(T, M), M > H.

% PARTE B
% --------------------------

largoAc([], N, N) .
largoAc([_|T] , Ac, N) :-
  Ac2 is Ac+1 ,
  largoAc(T, Ac2, N).

maximoAc([H], H, H).
maxci