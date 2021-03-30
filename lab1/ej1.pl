% --------------------------
% Ejercicio 1
% --------------------------

% Predicados auxiliares
% --------------------------

largo([], 0).
largo([_|T], X) :- largo(T, X2), X is X2 + 1.

enesimo([H|_], 1, H).
enesimo([_|T], N, X) :- M is N-1, enesimo(T, M, X).

agregar_atras([], X, [X]).
agregar_atras([H|T], X, [H|L]) :- agregar_atras(T, X, L).

agregar_adelante([], X, [X]).
agregar_adelante([H|T], X, [X,H|T]).

generar_transpuesta(M, 1, X) :- col(M, 1, C), agregar_atras([], C, X).
generar_transpuesta(M, N, X) :- col(M, N, C), N2 is N-1, generar_transpuesta(M, N2, X2), agregar_atras(X2, C, X).

% Predicados principales
% --------------------------

% elegir(?X,?L1,?L2) - La lista L2 resulta de eliminar un elemento de la lista L1.
elegir(X, [X|T], T).
elegir(X, [H|T], [H|L]) :- elegir(X, T, L).

% elegirN(+L,+L1,+N,?L2) - La lista L2 resulta de eliminar N elementos de la lista L1. La lista L tiene los elementos eliminados en orden.
elegirN([X], L1, 1, L2) :- elegir(X, L1, L2).
elegirN([H|T], L1, N, L2) :- elegir(H, L1, L), M is N-1, elegirN(T, L, M, L2).

% suma(+L,?S) - S es la suma de los elementos de la lista L.
suma([X], X).
suma([H|T], X) :- suma(T, X2), X is H + X2.

% matriz(+M,+N,?A) - A es una matriz de M filas y N columnas. La matriz se representa mediante una lista de M filas, donde cada fila es una lista de N celdas.
matriz(1, N, [[H|T]]) :- largo([H|T], N).
matriz(M, N, [[H1|T1]|T2]) :- largo([H1|T1], N), M2 is M-1, matriz(M2, N, T2).

% valor_celda(+I,+J,+A,?E) - E es el contenido de la celda (I,J) de la matriz A
valor_celda(1, J, [H|_], E) :- enesimo(H, J, E).
valor_celda(I, J, [_|T], E) :- M is I-1, valor_celda(M, J, T, E).

% fila(+M,?N,?F) - F es la fila N-ésima de la matriz
fila([H|_], 1, H).
fila([_|T], N, F) :- M is N-1, fila(T, M, F).

% col(+M,?N,?C) - C es la columna N-ésima de la matriz
col([H], N, C) :- enesimo(H, N, X), agregar_adelante([], X, C).
col([H|T], N, C) :- col(T, N, C2), enesimo(H, N, X), agregar_adelante(C2, X, C).

% transpuesta(?M, ?T) - T es la matriz transpuesta de M
transpuesta([H|T], X) :- largo(H, N), generar_transpuesta([H|T], N, X).

% numeros(+Inicio,+Fin,Lista) ← Lista es una lista ordenada de los números entre Inicio y Fin. Si Inicio es mayor que fin, el predicado falla.
numeros(F,F,[F]).
numeros(I,F,[I|L]) :- I < F, I2 is I+1, numeros(I2, F, L).