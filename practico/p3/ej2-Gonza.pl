% 
% El   problema   del   «ataque   de   las  k  reinas»   consiste en distribuir k reinas 
% en un tablero de  n  por  n, de forma quetoda casilla del tablero quede atacada por una reina,
% y ninguna reina sea atacada por otra.

% Definicion del Tablero
% ----------------------------------------------------------------------------------------------------------

% pos(X, Y)
pos(_, _).

% kreinas(K,N,Reinas) -> Reinas es una solución alproblema del ataque de las K reinas en un tablero de tamaño NxN.

% Predicados Auxiliares
% ----------------------------------------------------------------------------------------------------------
tablero(N, [pos(N,N)|L], [pos(N,N)|L]).
tablero(N, [pos(F,N)|L], T) :- F < N, F2 is F + 1, tablero(N, [pos(F2,1),pos(F,N)|L], T).
tablero(N, [pos(F,C)|L], T) :- C < N, C2 is C + 1, tablero(N, [pos(F,C2),pos(F,C)|L], T).

cubrirFila(_, [], []).
cubrirFila(pos(X, _), [pos(X, _) | L] , L2) :- cubrirFila(pos(X, _), L , L2), !.
cubrirFila(pos(X, _),  [H | L], [H | L2]) :- cubrirFila(pos(X, _), L , L2).

cubrirColumna(_, [], []).
cubrirColumna(pos(_, Y), [pos(_, Y) | L] , L2) :- cubrirColumna(pos(_, Y),  L, L2), !.
cubrirColumna(pos(_, Y), [H | L] , [H | L2]) :- cubrirColumna(pos(_, Y),  L, L2).

estaEnDiagonal(pos(X, Y), pos(X1, Y1), It, _) :-
    X1 is X + It,
    Y1 is Y + It,
    !.
estaEnDiagonal(pos(X, Y), pos(X1, Y1), It, _) :-
    X1 is X - It,
    Y1 is Y + It,
    !.
estaEnDiagonal(pos(X, Y), pos(X1, Y1), It, _) :-
    X1 is X + It,
    Y1 is Y - It,
    !.
estaEnDiagonal(pos(X, Y), pos(X1, Y1), It, _) :-
    X1 is X - It,
    Y1 is Y - It,
    !.
estaEnDiagonal(pos(X, Y), pos(X1, Y1), It, N) :-
    It < N,
    It2 is It + 1,
    estaEnDiagonal(pos(X, Y), pos(X1, Y1), It2, N).
    

% cubrirDiagonales(pos(X, Y), CasillasLibres, NuevasCasillasLibres).
cubrirDiagonales(_, [], _, []).
cubrirDiagonales(pos(X, Y), [pos(X1, Y1) | L], N, L2) :- 
    estaEnDiagonal(pos(X, Y), pos(X1, Y1), 1, N), 
    !, 
    cubrirDiagonales(pos(X, Y), L, N, L2).
cubrirDiagonales(pos(X, Y), [H | L], N, [H | L2]) :- 
    cubrirDiagonales(pos(X, Y), L, N, L2).

cubrirCasillas(pos(X, Y), CasillasLibres, N, NuevasCasillasLibres3) :-
    cubrirFila(pos(X, Y), CasillasLibres, NuevasCasillasLibres),
    cubrirColumna(pos(X, Y), NuevasCasillasLibres, NuevasCasillasLibres2),
    cubrirDiagonales(pos(X, Y), NuevasCasillasLibres2, N, NuevasCasillasLibres3).

colocarReinas(0,_, _, Reinas, Reinas, _).
colocarReinas(K, N, CasillasLibres, ReinasAcumuladas, Reinas, NuevasCasillasLibres2) :-
    select(NuevaReina, CasillasLibres, CasillasLibres2),
  % write(K), write('  '), write(NuevaReina), wtite(' \\ '),
   % write(K), write('  '), write(NuevaReina), write('  '), write(CasillasLibres2), write(' \\ '), 
    cubrirCasillas(NuevaReina, CasillasLibres2, N, NuevasCasillasLibres),
     K1 is K - 1,
  %  write(K), write('  '), write( [NuevaReina | ReinasAcumuladas]), write(' \\ '),
    colocarReinas(K1, N, NuevasCasillasLibres, [NuevaReina | ReinasAcumuladas], Reinas, NuevasCasillasLibres2).

% Predicado Principal
% ----------------------------------------------------------------------------------------------------------
kreinas(K,N,Reinas):-
    tablero(N, [pos(1,1)], CasillasLibres),
    colocarReinas(K, N, CasillasLibres, [], Reinas, []).