% --------------------------
% Ejercicio 1
% --------------------------

% Predicados auxiliares
% --------------------------

% crear_fila(+N, ?E, ?F) - Comprueba si la lista F tiene largo N y E elementos
crear_fila(1, E, [E]).
crear_fila(N, E, [E|T]) :- N > 1, N2 is N-1, crear_fila(N2, E, T).

% generar_transpuesta(+A, +L, +N, ?C) - Genera matriz transpuesta A a partir de matriz C, siendo L un acumulador para iterar y N la cantidad de filas de C
generar_transpuesta(A, N, N, [C]) :- col(N, A, C).
generar_transpuesta(A, L, N, [C1|C2]) :- col(L, A, C1), L2 is L+1, generar_transpuesta(A, L2, N, C2).

% largo(+L, ?N) - Comprueba si la lista L tiene N elementos
largo([], 0).
largo([_|T], X) :- largo(T, X2), X is X2 + 1.

% Predicados principales
% --------------------------

% elegir(?X,?L1,?L2) - La lista L2 resulta de eliminar un elemento de la lista L1.
elegir(X, [X|T], T).
elegir(X, [H|T], [H|L]) :- elegir(X, T, L).

% elegirN(+L,+L1,+N,?L2) - La lista L2 resulta de eliminar N elementos de la lista L1. La lista L tiene los elementos eliminados en orden de selección.
elegirN([], L1, 0, L1).
elegirN([H|T], L1, N, L2) :- elegir(H, L1, L), M is N-1, elegirN(T, L, M, L2).

% suma(+L,?S) - S es la suma de los elementos de la lista L.
suma([], 0).
suma([H|T], X) :- suma(T, X2), X is H + X2.

% matriz(+M,+N,+E,?A) - A es una matriz de M filas y N columnas, donde cada celda tiene el elemento E.
% La matriz se representa mediante una lista de M filas, donde cada fila es una lista de N celdas.
matriz(1, N, E, [F]) :- crear_fila(N, E, F).
matriz(M, N, E, [F|T2]) :- M > 1, crear_fila(N, E, F), M2 is M-1, matriz(M2, N, E, T2).

% fila(+N,+M,+F) - F es la fila N-ésima de la matriz
fila(1, [H|_], H).
fila(N, [_|T], F) :- M is N-1, fila(M, T, F).

% valor_celda(+I,+J,+A,?E) - E es el contenido de la celda (I,J) de la matriz A
valor_celda(1, J, [H|_], E) :- fila(J, H, E).
valor_celda(I, J, [_|T], E) :- M is I-1, valor_celda(M, J, T, E).

% col(+N,+M,?C) - C es la columna N-ésima de la matriz
col(N, [H], [X]) :- fila(N, H, X).
col(N, [H|T], [C1|C2]) :- fila(N, H, C1), col(N, T, C2).

% transpuesta(+M,?T) - T es la matriz transpuesta de M
transpuesta([[]], [[]]).
transpuesta([H|T], X) :- largo(H, N), generar_transpuesta([H|T], 1, N, X).

% numeros(+Inicio,+Fin,Lista) ← Lista es una lista ordenada de los números entre Inicio y Fin. Si Inicio es mayor que fin, el predicado falla.
numeros(F,F,[F]).
numeros(I,F,[I|L]) :- I < F, I2 is I+1, numeros(I2, F, L).

% --------------------------
% Ejercicio 2
% --------------------------

% Predicados auxiliares
% --------------------------

% depurarH(+H, +L, ?L2). <- La lista L2 resulta de eliminar todos los numeros multiplos de H en la lista L
depurarH(_, _, [], []).
depurarH(H, N, [H1|L], L2) :- 0 is mod(H1, H), depurarH(H, N, L, L2).
depurarH(H, N, [H1|L], [H1|L2]) :- not(0 is mod(H1, H)), depurarH(H, N, L, L2).

% depurar(+L1, ?L2) <- La lista L2 es la lista L1 que solo contiene a los primos.
depurar(N, [H|L], [H|L]) :- H2 is H * H,  not(H2 < N).
depurar(N, [H|L], [H|L2]) :- H2 is H * H, H2 < N, depurarH(H, N, L, L1), depurar(N, L1, L2).

% Predicados principales
% --------------------------
% primos(+N, ?P) <- P es la lista de todos los primos mayores a 1 y menores que N.
primos(0, []).
primos(1, []).
primos(2, []).
primos(N, P) :- N1 is N - 1, numeros(2, N1, L), depurar(N, L, P).

% --------------------------
% Ejercicio 3
% --------------------------

% Predicados auxiliares
% --------------------------

pos(_,_).

% pertenece(?X, ?L) - Comprueba si el elemento X pertenece a la lista L
pertenece(X, [X|_]).
pertenece(X, [_|T]):- pertenece(X,T).

% tablero(+N, +Ac, ?T) -> T es una lista de NxN con todas las celdas del tablero de NxN
tablero(N, [pos(N,N)|L], [pos(N,N)|L]).
tablero(N, [pos(F,N)|L], T) :- F < N, F2 is F + 1, tablero(N, [pos(F2,1),pos(F,N)|L], T). 
tablero(N, [pos(F,C)|L], T) :- C < N, C2 is C + 1, tablero(N, [pos(F,C2),pos(F,C)|L], T). 

% adyacente(+A, ?B, +N) -> sean A y B dos posiciones de la forma pos(Fila, Columna). B es una celda adyacente a A. Es decir que comparten fila o columna con distancia de 1. N es el limite de la tabla.
adyacente(pos(F,C1), pos(F, C2), _) :- C2 is C1 - 1, C2>0.
adyacente(pos(F,C1), pos(F, C2), N) :- C2 is C1 + 1, C1<N.
adyacente(pos(F1,C), pos(F2, C), _) :- F2 is F1 - 1, F2>0.
adyacente(pos(F1,C), pos(F2, C), N) :- F2 is F1 + 1, F1<N.

% secuencia(+Inicial, +Celdas, +Muros, +N, +Acumulador, ?Caminito) -> Caminito es una secuencia de posiciones de la forma pos(Fila,Columna), dentro de la matriz NxN
%                                                                     comienza en Inicial y termina en Final.
secuencia(Inicial, _, _, _, [A|L], [A|L]):- 
    A = Inicial.

secuencia(Inicial, Celdas, Muros, N, [A|L], Caminito):- 
    adyacente(A, Ady, N),
    not(pertenece(Ady,Muros)),
    pertenece(Ady,Celdas),
    elegir(Ady, Celdas, NCeldas),
    secuencia(Inicial, NCeldas, Muros, N, [Ady,A|L], Caminito).

% Predicados principales
% --------------------------

% caminito(+N,+Muros,+Inicial,+Final,?Caminito) ->  Caminito es una secuencia de posiciones de la forma pos(Fila,Columna)
%                                                   , correspondiente a un camino entre   la   casilla  Inicial  y   la   casilla  Final,
%                                                   ambas   denotadas   también   comopos(Fila,Columna).
%                                                   La lista de  Muros  está especificada también como una lista de posiciones.
caminito(N, _, Inicial, Inicial, [Inicial]):-  N > 0.
caminito(N, Muros, Inicial, Final, Caminito) :-
    not(pertenece(Final, Muros)),
    not(pertenece(Inicial, Muros)),
    tablero(N, [pos(1,1)], T),
    elegir(Final, T, Celdas),
    secuencia(Inicial, Celdas, Muros, N, [Final], Caminito).
