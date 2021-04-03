% --------------------------
% Ejercicio 2
% --------------------------

% Predicados auxiliares
% --------------------------
% pertenece(?X, ?L) <- true si X se encuentra en L.
pertenece(X,[X|_]).
pertenece(X,[_|T]):- pertenece(X,T).

% borrarH(+H, +L1, ?L2) <- L2 resulta de eliminar todas las ocurrencias H de la lista L2
borrarH(_, [], []). 
borrarH(H, [H|L1], L2) :- borrarH(H, L1, L2).
borrarH(H, [Y|L1], [Y|L2]) :- Y \= H, borrarH(H, L1, L2).

% borrar(+L, +L1, ?L2) <- L2 resulta de eliminar los elementos de L que se encuentren en L1
borrar([], L, L).
borrar([H|L], L1, L2) :- borrarH(H, L1, L3), borrar(L, L3, L2).

% numeros(+Inicio,+Fin, ?Lista) ← Lista es una lista ordenada de los números entre Inicio y Fin. Si Inicio es mayor que fin, el predicado falla.
numeros(F,F,[]).
numeros(I,F,[I|L]) :- I < F, I2 is I+1, numeros(I2, F, L).

% calculo_multiplos(+H, +N, +L, ?L2) <- L2 es la lista de todos los multiplos de H, mayores a H y menores a N 
calculo_multiplos(H, N, [], []) :- H2 is H + H, H2 >= N.
calculo_multiplos(H, N, [], L) :- H2 is H + H, H2 < N, calculo_multiplos(H, N, [H2], L).
calculo_multiplos(H, N, [H1|L1], L) :- H2 is H1 + H, H2 < N, calculo_multiplos(H, N, [H2,H1|L1], L).
calculo_multiplos(H, N, [H1|L1], [H1|L1]) :- H2 is H1 + H, H2 >= N.

% depurarH(+H, +L, ?L2). <- La lista L2 resulta de eliminar todos los numeros multiplos de H en la lista L
depurarH(_, _, [], []).
depurarH(H, N, [H1|L], L2) :- calculo_multiplos(H, N, [], M), borrar(M, [H1|L], L2).

% depurar(+L1, ?L2) <- La lista L2 es la lista L1 que solo contiene a los primos.
depurar(N, [H|L], [H|L]) :- H2 is H * H,  not(H2 < N).
depurar(N, [H|L], [H|L2]) :- H2 is H * H, H2 < N, depurarH(H, N, L, L1), depurar(N, L1, L2).


% Predicados principales
% --------------------------
% primos(+N, ?P) <- P es la lista de todos los primos mayores a 1 y menores que N.
primos(0, []).
primos(1, []).
primos(2, []).
primos(N, P) :- numeros(2, N, L), depurar(N, L, P).