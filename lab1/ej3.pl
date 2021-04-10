% --------------------------
% Ejercicio 3
% --------------------------

% Predicados auxiliares
% --------------------------
pos(_,_).

% select(?X, ?L, ?LsinX) -> LsinX es la lista L sin el elemento X
select(X,[X|Xs],Xs). 
select(X,[Y|Xs],[Y|R]) :- select(X,Xs,R).

% tablero(+N, +Ac, ?T) -> T es una lista de NxN con todas las celdas del tablero de NxN
tablero(N, [pos(N,N)|L], [pos(N,N)|L]).
tablero(N, [pos(F,N)|L], T) :- F < N, F2 is F + 1, tablero(N, [pos(F2,1),pos(F,N)|L], T). 
tablero(N, [pos(F,C)|L], T) :- C < N, C2 is C + 1, tablero(N, [pos(F,C2),pos(F,C)|L], T). 

% pertenece(?X, ?L) -> devuelve true si X pertenece a la lista L 
pertenece(X,[X|_]).
pertenece(X,[_|T]):- pertenece(X,T).

% adyacente(+A, ?B, +N) -> sean A y B dos posiciones de la forma pos(Fila, Columna). B es una celda adyacente a A. Es decir que comparten fila o columna con distancia de 1. N es el limite de la tabla.
adyacente(pos(F,C1), pos(F, C2),_) :- C2 is C1 - 1, C2>0.
adyacente(pos(F,C1), pos(F, C2),N) :- C2 is C1 + 1, C1<N.
adyacente(pos(F1,C), pos(F2, C),_) :- F2 is F1 - 1, F2>0.
adyacente(pos(F1,C), pos(F2, C),N) :- F2 is F1 + 1, F1<N.


% secuencia(+Inicial, +Celdas, +Muros, +N, +Acumulador, ?Caminito) -> Caminito es una secuencia de posiciones de la forma pos(Fila,Columna), dentro de la matriz NxN
%                                                                     comienza en Inicial y termina en Final.
secuencia(Inicial, _, _, _, [A|L], [A|L]):- 
    A = Inicial.

secuencia(Inicial, Celdas, Muros, N, [A|L], Caminito):- 
    adyacente(A, Ady, N),
    not(pertenece(Ady,Muros)),
    pertenece(Ady,Celdas),
    select(Ady, Celdas, NCeldas),
    secuencia(Inicial, NCeldas, Muros, N, [Ady,A|L], Caminito).

% Predicados principales
% --------------------------

% caminito(+N,+Muros,+Inicial,+Final,?Caminito) ->  Caminito es una secuencia de posiciones de la forma pos(Fila,Columna)
%                                                   , correspondiente a un camino entre   la   casilla  Inicial  y   la   casilla  Final,
%                                                   ambas   denotadas   también   comopos(Fila,Columna).
%                                                   La lista de  Muros  está especificada también como una lista de posiciones.
caminito(N, _, Inicial, Inicial, [Inicial]):-  N > 0.
caminito(N, Muros, Inicial, Final, Caminito) :-
    tablero(N, [pos(1,1)], T),
    select(Final, T, Celdas),
    not(pertenece(Final, Muros)),
    not(pertenece(Inicial, Muros)),
    secuencia(Inicial, Celdas, Muros, N, [Final], Caminito).