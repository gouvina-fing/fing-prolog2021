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

% es_adyacente(+A, ?B) -> sean A y B dos posiciones de la forma pos(Fila, Columna). B es una celda adyacente a A. Es decir que comparten fila o columna con distancia de 1.
es_adyacente(pos(F,C1), pos(F, C2)) :- C2 is C1 - 1.
es_adyacente(pos(F,C1), pos(F, C2)) :- C2 is C1 + 1.
es_adyacente(pos(F1,C), pos(F2, C)) :- F2 is F1 - 1.
es_adyacente(pos(F1,C), pos(F2, C)) :- F2 is F1 + 1.

% siguiente_paso(+L, ?H) -> H es la cabeza de L
siguiente_paso([H|_], H).

% secuencia(+Inicial, +Final, +SinColocar, +Acumulador, ?Caminito) -> Caminito es una secuencia de posiciones de la forma pos(Fila,Columna), dentro de la matriz NxN
%                                               comienza en Inicial y termina en Final
% de esta forma logro una secuencia que siempre empieza en Inicial, termina en Final, y no pasa 2 veces por la misma celda
secuencia(Inicial, _, _, [H|L], [Inicial, H|L] ).
secuencia(Inicial, Final, SinColocar, [], Secuencia ) :- secuencia(Inicial, Final, SinColocar, [Final], Secuencia ).
secuencia(Inicial, _, SinColocar, [H|L], Secuencia ) :- select(X, SinColocar, NuevoSinColocar), secuencia(Inicial, _, NuevoSinColocar, [X, H|L], Secuencia ). 

% chequeoCamino(Muros, Final, Caminito) ->  Devuelve true si Caminito es un camino posible que termine en Final. 
chequeoCamino(Muros, Final, [Final]) :- not(pertenece(Final, Muros)).
chequeoCamino(Muros, Final, [H|L]) :- 
    H \= Final,
    not(pertenece(H, Muros)), 
    siguiente_paso(L, X),
    es_adyacente(H, X),
    chequeoCamino(Muros, Final, L).

% --------------------------
% Predicados principales
% --------------------------

% caminito(+N,+Muros,+Inicial,+Final,?Caminito) ->  Caminito es una secuencia de posiciones de la forma pos(Fila,Columna)
%                                                   , correspondiente a un camino entre   la   casilla  Inicial  y   la   casilla  Final,
%                                                   ambas   denotadas   también   comopos(Fila,Columna).
%                                                   La lista de  Muros  está especificada también como una lista deposiciones.
caminito(N, _, Inicial, Inicial, [Inicial]):-  N > 0.
caminito(N, Muros, Inicial, Final, Caminito) :-
    tablero(N, [pos(1,1)], T),
    select(Final, T, NuevoT),
    select(Inicial, NuevoT, Celdas),
    secuencia(Inicial, Final, Celdas, [], Caminito),
    chequeoCamino(Muros, Final, Caminito).


