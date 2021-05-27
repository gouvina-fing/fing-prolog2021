:- module(ai,
[
    % GENERALES
    hacer_movimiento_fase1/2,
    % MINIMAX
    pre_minimax/2,
    minimax/7
]).

:- use_module(core).

%% PREDICADOS GENERALES
%% ----------------------------------------------------------------------------------------------------------------------------------------------

% hacer_movimiento(+Tablero, +Jugador, +Estrategia) -> VERSION DUMMY, FASE 1
hacer_movimiento_fase1(Tablero, Jugador, dummy) :-
    % Elegir ficha 1
    valor_celda(Tablero, I, J, -),
    not(es_centro(I, J)),
    modificar_celda(Tablero, I, J, Jugador),
    % Elegir ficha 2
    valor_celda(Tablero, I2, J2, -),
    not(es_centro(I2, J2)),
    modificar_celda(Tablero, I2, J2, Jugador).

%% PREDICADOS MINIMAX
%% ----------------------------------------------------------------------------------------------------------------------------------------------

%
pre_minimax(99999, -99999).

%
% Paso Base -> Nivel = 0
minimax(0, _Alpha, _Beta, Jugador, EstadoFinal, EstadoFinal, Score) :-
    calcular_puntaje_minimax_hoja(Jugador, EstadoFinal, Score), !.
% Paso Base -> Nivel > 0, Fin de juego
minimax(Nivel, _Alpha, _Beta, Jugador, EstadoFinal, EstadoFinal, Score) :-
    Nivel > 0,
    chequear_final(Jugador, EstadoFinal),
    calcular_puntaje_minimax_hoja(Jugador, EstadoFinal, Score), !.
% Paso Inductivo -> 
minimax(Nivel, Alpha, Beta, Jugador, EstadoBase, EstadoFinal, Score) :-
    calcular_posibles_estados(Jugador, EstadoBase, Estados). % Aca se hacen todos los movimientos posibles
    calcular_puntaje_minimax_rama(Nivel, Alpha, Beta, Jugador, EstadoBase, Estados, EstadoFinal, Score).

%
% Paso Base -> 
calcular_puntaje_minimax_rama(Nivel, Alpha, Beta, Jugador, _, [Estado], EstadoFinal, Score) :-
    Nivel2 is Nivel - 1,
    jugador_opuesto(Jugador, Contrincante), 
    minimax(Nivel2, Alpha, Beta, Contrincante, Estado, EstadoFinal, Score), !.
% Paso Inductivo ->
calcular_puntaje_minimax_rama(Nivel, Alpha, Beta, Jugador, EstadoBase, [Estado | Estados], EstadoFinal, Score) :-
    Nivel2 is Nivel - 1,
    jugador_opuesto(Jugador, Contrincante),
    minimax(Nivel2, Alpha, Beta, Contrincante, Estado, EstadoFinal2, Score2),
    calcular_alpha_beta(Alpha, Beta, Score2, Alpha2, Beta2)
    (
        Alpha >= Beta
        ->
            EstadoFinal = EstadoFinal2, % REVISAR
            Score = Score2,
            !
        ;
            calcular_puntaje_minimax_rama(Nivel, Alpha2, Beta2, Jugador, Estado, Estados, EstadoFinal, Score)
    )
