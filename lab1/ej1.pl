% --------------------------
% Ejercicio 1
% --------------------------

% Predicados auxiliares
% --------------------------

% pertenece(?X,?L) - Comprueba si el elemento X pertenece a la lista L
pertenece(X,[X|_]).
pertenece(X,[_|T]):- pertenece(X,T).

% largo(?L,?N) - 
largo([], 0).
largo([_|T], X) :- largo(T, X2), X is X2 + 1.

% pertenece(?X,?L) - 
crear_fila(1, E, [E]).
crear_fila(N, E, [E|T]) :- N > 1, N2 is N-1, crear_fila(N2, E, T).

% pertenece(?X,?L) - 
enesimo([H|_], 1, H).
enesimo([_|T], N, X) :- M is N-1, enesimo(T, M, X).

% pertenece(?X,?L) - 
generar_transpuesta(M, L, L, [C]) :- col(L, M, C).
generar_transpuesta(M, N, L, [C1|C2]) :- col(N, M, C1), N2 is N+1, generar_transpuesta(M, N2, L, C2).


% Predicados principales
% --------------------------

% elegir(?X,?L1,?L2) - La lista L2 resulta de eliminar un elemento de la lista L1.
elegir(_, [], []).
elegir(X, [X|T], T).
elegir(X, [H|T], [H|L]) :- pertenece(X, T), elegir(X, T, L).

% elegirN(+L,+L1,+N,?L2) - La lista L2 resulta de eliminar N elementos de la lista L1. La lista L tiene los elementos eliminados en orden.
elegirN([], L1, 0, L1).
elegirN([H|T], L1, N, L2) :- elegir(H, L1, L), M is N-1, elegirN(T, L, M, L2).
% REVISAR-------------------------------------

% suma(+L,?S) - S es la suma de los elementos de la lista L.
suma([], 0).
suma([H|T], X) :- suma(T, X2), X is H + X2.

% matriz(+M,+N, +E, ?A) - A es una matriz de M filas y N columnas, donde cada celda tiene el elemento E.
% La matriz se representa mediante una lista de M filas, donde cada fila es una lista de N celdas.
matriz(1, N, E, [F]) :- crear_fila(N, E, F).
matriz(M, N, E, [F|T2]) :- M > 1, crear_fila(N, E, F), M2 is M-1, matriz(M2, N, E, T2).


% valor_celda(+I,+J,+A,?E) - E es el contenido de la celda (I,J) de la matriz A
valor_celda(1, J, [H|_], E) :- enesimo(H, J, E).
valor_celda(I, J, [_|T], E) :- M is I-1, valor_celda(M, J, T, E).

% OTRA FORMA
% valor_celda(I,J,A,E) :- fila(I,A,F), enesimo(F, J, E).

% fila(?N,+M,?F) - F es la fila N-ésima de la matriz
fila(1, [H|_], H).
fila(N, [_|T], F) :- M is N-1, fila(T, M, F).

% col(?N, +M,?C) - C es la columna N-ésima de la matriz
col( N, [H], [X]) :- enesimo(H, N, X).
col( N, [H|T], [C1|C2]) :- enesimo(H, N, C1), col(T, N, C2).

% transpuesta(?M, ?T) - T es la matriz transpuesta de M
transpuesta([[]], [[]]).
transpuesta([H|T], X) :- largo(H, N), generar_transpuesta([H|T], 1, N, X).

% numeros(+Inicio,+Fin,Lista) ← Lista es una lista ordenada de los números entre Inicio y Fin. Si Inicio es mayor que fin, el predicado falla.
numeros(F,F,[F]).
numeros(I,F,[I|L]) :- I < F, I2 is I+1, numeros(I2, F, L).