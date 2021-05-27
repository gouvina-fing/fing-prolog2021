:- module(ai,
[
    % GENERALES
    hacer_movimiento_fase1/2,
    % MINIMAX
    pre_minimax/2,
    minimax/8
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

%% PREDICADOS PRINCIPALES MINIMAX
%% ----------------------------------------------------------------------------------------------------------------------------------------------

%
% Paso Base -> Nivel = 0
minimax(0, _Alpha, _Beta, _Estrategia, Jugador, EstadoFinal, EstadoFinal, Puntaje) :-
    calcular_puntaje_minimax_hoja(Jugador, EstadoFinal, Puntaje), !.
% Paso Base -> Nivel > 0, Fin de juego
minimax(_Nivel, _Alpha, _Beta, _Estrategia, Jugador, EstadoFinal, EstadoFinal, Puntaje) :-
    chequear_final(EstadoFinal),
    calcular_puntaje_minimax_hoja(Jugador, EstadoFinal, Puntaje), !.
% Paso Inductivo -> 
minimax(Nivel, Alpha, Beta, Estrategia, Jugador, EstadoBase, EstadoFinal, Puntaje) :-
    calcular_posibles_estados(Jugador, EstadoBase, Estados). % Aca se hacen todos los movimientos posibles
    calcular_puntaje_minimax_rama(Nivel, Alpha, Beta, Estrategia, Jugador, EstadoBase, Estados, EstadoFinal, Puntaje).

%
% Paso Base -> 
calcular_puntaje_minimax_rama(Nivel, Alpha, Beta, Estrategia, Jugador, _, [Estado], EstadoFinal, Puntaje) :-
    Nivel2 is Nivel - 1,
    jugador_opuesto(Jugador, Contrincante), 
    estrategia_opuesta(Estrategia, Estrategia2),
    minimax(Nivel2, Alpha, Beta, Estrategia2, Contrincante, Estado, EstadoFinal, Puntaje), !.
% Paso Inductivo ->
calcular_puntaje_minimax_rama(Nivel, Alpha, Beta, Estrategia, EstadoBase, [Estado | Estados], EstadoFinal, Puntaje) :-
    Nivel2 is Nivel - 1,
    jugador_opuesto(Jugador, Contrincante),
    estrategia_opuesta(Estrategia, Estrategia2),
    minimax(Nivel2, Alpha, Beta, Estrategia2, Contrincante, Estado, EstadoFinal2, Puntaje2),
    calcular_alpha_beta(Alpha, Beta, Estrategia2, Puntaje2, Alpha2, Beta2)
    (
        Alpha2 >= Beta2
        ->
            EstadoFinal = EstadoFinal2, % REVISAR
            Puntaje = Puntaje2,
            !
        ;
            calcular_puntaje_minimax_rama(Nivel, Alpha2, Beta2, Estrategia, Jugador, Estado, Estados, EstadoFinal, Puntaje)
    )

%% PREDICADOS AUXILIARES MINIMAX
%% ----------------------------------------------------------------------------------------------------------------------------------------------

%
pre_minimax(-99999, 99999).