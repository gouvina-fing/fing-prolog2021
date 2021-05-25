:- module(ai,
[
    % GENERALES
    hacer_movimiento_1/2
]).

:- use_module(core).

%% PREDICADOS AUXILIARES
%% ----------------------------------------------------------------------------------------------------------------------------------------------

elegir_fase_1(Tablero, Jugador) :-
    valor_celda(Tablero, I, J, -),
    not(es_centro(I, J)),
    modificar_celda(Tablero, I, J, Jugador).

hacer_movimiento_1(Tablero, Jugador) :-
    elegir_fase_1(Tablero, Jugador),
    elegir_fase_1(Tablero, Jugador).

   